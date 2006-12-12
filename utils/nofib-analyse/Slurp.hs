-----------------------------------------------------------------------------
--
-- (c) Simon Marlow 1997-2005
--
-----------------------------------------------------------------------------

module Slurp (Status(..), Results(..), ResultTable, parse_log) where

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Regex
import Data.Maybe
-- import Debug.Trace

-----------------------------------------------------------------------------
-- This is the structure into which we collect our results:

type ResultTable = Map String Results

data Status
	= NotDone
	| Success
	| OutOfHeap
	| OutOfStack
	| Exit Int
	| WrongStdout
	| WrongStderr 

data Results = Results { 
	compile_time   	:: Map String Float,
	module_size	:: Map String Int,
	binary_size    	:: Maybe Int,
	link_time      	:: Maybe Float,
	run_time       	:: [Float],
	mut_time	:: [Float],
	instrs          :: Maybe Integer,
	mem_reads       :: Maybe Integer,
	mem_writes      :: Maybe Integer,
	cache_misses    :: Maybe Integer,
	gc_work        	:: Maybe Integer,
	gc_time        	:: [Float],
	allocs         	:: Maybe Integer,
	run_status     	:: Status,
	compile_status 	:: Status
	}

emptyResults :: Results
emptyResults = Results { 
	compile_time   	= Map.empty,
	module_size    	= Map.empty,
	binary_size    	= Nothing,
	link_time      	= Nothing,
	run_time       	= [],
	mut_time       	= [],
	instrs          = Nothing,
	mem_reads       = Nothing,
	mem_writes      = Nothing,
	cache_misses    = Nothing,
	gc_time        	= [],
	gc_work        	= Nothing,
	allocs	       	= Nothing,
	compile_status 	= NotDone,
	run_status     	= NotDone
	}

-----------------------------------------------------------------------------
-- Parse the log file

{-
Various banner lines:

==nofib== awards: size of QSort.o follows...
==nofib== banner: size of banner follows...
==nofib== awards: time to link awards follows...
==nofib== awards: time to run awards follows...
==nofib== boyer2: time to compile Checker follows...
-}

-- NB. the hyphen must come last (or first) inside [...] to stand for itself.
banner_re :: Regex
banner_re = mkRegex "^==nofib==[ \t]+([A-Za-z0-9_-]+):[ \t]+(size of|time to link|time to run|time to compile)[ \t]+([A-Za-z0-9_-]+)(\\.o)?[ \t]+follows"

{-
This regexp for the output of "time" works on FreeBSD, other versions
of "time" will need different regexps.
-}

time_re :: Regex
time_re = mkRegex "^[ \t]*([0-9.]+)[ \t]+real[ \t]+([0-9.]+)[ \t]+user[ \t]+([0-9.]+)[ \t]+sys[ \t]*$"

time_gnu17_re :: Regex
time_gnu17_re = mkRegex "^[ \t]*([0-9.]+)user[ \t]+([0-9.]+)system[ \t]+([0-9.:]+)elapsed"
                -- /usr/bin/time --version reports: GNU time 1.7
                -- notice the order is different, and the elapsed time is [hh:]mm:ss.s

size_re :: Regex
size_re = mkRegex "^[ \t]*([0-9]+)[ \t]+([0-9]+)[ \t]+([0-9]+)"

{-
<<ghc: 5820820 bytes, 0 GCs, 0/0 avg/max bytes residency (0 samples), 41087234 bytes GC work, 0.00 INIT (0.05 elapsed), 0.08 MUT (0.18 elapsed), 0.00 GC (0.00 elapsed) :ghc>>

	= (bytes, gcs, avg_resid, max_resid, samples, gc_work,
	   init, init_elapsed, mut, mut_elapsed, gc, gc_elapsed)

ghc1_re = pre GHC 4.02
ghc2_re = GHC 4.02 (includes "xxM in use")
ghc3_re = GHC 4.03 (includes "xxxx bytes GC work")
-}

ghc1_re, ghc2_re, ghc3_re, ghc4_re :: Regex
ghc1_re = mkRegex "^<<ghc:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+) bytes GC work, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\) :ghc>>"

ghc2_re = mkRegex "^<<ghc:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+)M in use, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\) :ghc>>"

ghc3_re = mkRegex "^<<ghc:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+) bytes GC work, ([0-9]+)M in use, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\) :ghc>>"

ghc4_re = mkRegex "^<<ghc-instrs:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+) bytes GC work, ([0-9]+)M in use, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\), ([0-9]+) instructions, ([0-9]+) memory reads, ([0-9]+) memory writes, ([0-9]+) L2 cache misses :ghc-instrs>>"

wrong_exit_status, wrong_output, out_of_heap, out_of_stack :: Regex
wrong_exit_status = mkRegex "^\\**[ \t]*expected exit status ([0-9]+) not seen ; got ([0-9]+)"
wrong_output      = mkRegex "^expected (stdout|stderr) not matched by reality$"
out_of_heap       = mkRegex "^\\+ Heap exhausted;$"
out_of_stack      = mkRegex "^\\+ Stack space overflow:"

parse_log :: String -> ResultTable
parse_log
	= combine_results		-- collate information
	. concat
	. map process_chunk		-- get information from each chunk
	. tail				-- first chunk is junk
	. chunk_log [] []		-- break at banner lines
	. lines

combine_results :: [(String,Results)] -> Map String Results
combine_results = foldr f Map.empty
 where
	f (prog,results) fm = Map.insertWith (flip combine2Results) prog results fm

combine2Results :: Results -> Results -> Results
combine2Results
             Results{ compile_time = ct1, link_time = lt1, 
		      module_size = ms1,
		      run_time = rt1, mut_time = mt1, 
		      instrs = is1, mem_reads = mr1, mem_writes = mw1,
		      cache_misses = cm1,
		      gc_time = gt1, gc_work = gw1,
		      binary_size = bs1, allocs = al1, 
		      run_status = rs1, compile_status = cs1 }
	     Results{ compile_time = ct2, link_time = lt2, 
		      module_size = ms2,
		      run_time = rt2, mut_time = mt2,
		      instrs = is2, mem_reads = mr2, mem_writes = mw2,
		      cache_misses = cm2,
		      gc_time = gt2, gc_work = gw2,
		      binary_size = bs2, allocs = al2, 
		      run_status = rs2, compile_status = cs2 }
	  =  Results{ compile_time   = Map.unionWith (flip const) ct1 ct2,
		      module_size    = Map.unionWith (flip const) ms1 ms2,
		      link_time      = lt1 `mplus` lt2,
		      run_time       = rt1 ++ rt2,
		      mut_time       = mt1 ++ mt2,
		      instrs         = is1 `mplus` is2,
		      mem_reads      = mr1 `mplus` mr2,
		      mem_writes     = mw1 `mplus` mw2,
		      cache_misses   = cm1 `mplus` cm2,
		      gc_time        = gt1 ++ gt2,
		      gc_work        = gw1 `mplus` gw2,
		      binary_size    = bs1 `mplus` bs2,
		      allocs         = al1 `mplus` al2,
		      run_status     = combStatus rs1 rs2,
		      compile_status = combStatus cs1 cs2 }

combStatus :: Status -> Status -> Status
combStatus NotDone y       = y
combStatus x       NotDone = x
combStatus x       _       = x

chunk_log :: [String] -> [String] -> [String] -> [([String],[String])]
chunk_log header chunk [] = [(header,chunk)]
chunk_log header chunk (l:ls) =
	case matchRegex banner_re l of
		Nothing -> chunk_log header (l:chunk) ls
		Just stuff -> (header,chunk) : chunk_log stuff [] ls

process_chunk :: ([String],[String]) -> [(String,Results)]
process_chunk (progName : what : modName : _, chk) =
 case what of
	"time to compile" -> parse_compile_time progName modName chk
	"time to run"     -> parse_run_time progName (reverse chk) emptyResults NotDone
	"time to link"    -> parse_link_time progName chk
	"size of"	      -> parse_size progName modName chk
	_		          -> error ("process_chunk: "++what)
process_chunk _ = error "process_chunk: Can't happen"

parse_compile_time :: String -> String -> [String] -> [(String, Results)]
parse_compile_time _    _   [] = []
parse_compile_time progName modName (l:ls) =
	case matchRegex time_re l of {
	     Just (_real:user:_system:_) ->
		let ct  = Map.singleton modName (read user)
		in 
		[(progName, emptyResults{compile_time = ct})];
	     Nothing -> 

	case matchRegex time_gnu17_re l of {
	     Just (user:_system:_elapsed:_) ->
		let ct  = Map.singleton modName (read user)
		in 
		[(progName, emptyResults{compile_time = ct})];
	     Nothing -> 

	case matchRegex ghc1_re l of {
	    Just (_allocations:_:_:_:_:initialisation:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read initialisation + read_mut + read_gc) :: Float 
		  ct  = Map.singleton modName time
	      in
		[(progName, emptyResults{compile_time = ct})];
	    Nothing ->

	case matchRegex ghc2_re l of {
	   Just (_allocations:_:_:_:_:_:initialisation:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read initialisation + read_mut + read_gc) :: Float 
		  ct  = Map.singleton modName time
	      in
		[(progName, emptyResults{compile_time = ct})];
	    Nothing ->

	case matchRegex ghc3_re l of {
	   Just (_allocations:_:_:_:_:_:_:initialisation:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read initialisation + read_mut + read_gc) :: Float 
		  ct  = Map.singleton modName time
	      in
		[(progName, emptyResults{compile_time = ct})];
	    Nothing ->

	case matchRegex ghc4_re l of {
	   Just (_allocations:_:_:_:_:_:_:initialisation:_:mut:_:gc:_:_:_:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read initialisation + read_mut + read_gc) :: Float 
		  ct  = Map.singleton modName time
	      in
		[(progName, emptyResults{compile_time = ct})];
	    Nothing ->

		parse_compile_time progName modName ls
	}}}}}}

parse_link_time :: String -> [String] -> [(String, Results)]
parse_link_time _ [] = []
parse_link_time prog (l:ls) =
	  case matchRegex time_re l of {
	     Just (_real:user:_system:_) ->
		[(prog,emptyResults{link_time = Just (read user)})];
	     Nothing ->

	  case matchRegex time_gnu17_re l of {
	     Just (user:_system:_elapsed:_) ->
		[(prog,emptyResults{link_time = Just (read user)})];
	     Nothing ->

          parse_link_time prog ls
          }}


-- There might be multiple runs of the program, so we have to collect up
-- all the results.  Variable results like runtimes are aggregated into
-- a list, whereas the non-variable aspects are just kept singly.
parse_run_time :: String -> [String] -> Results -> Status
               -> [(String, Results)]
parse_run_time _ [] _ NotDone = []
parse_run_time prog [] res ex = [(prog, res{run_status=ex})]
parse_run_time prog (l:ls) res ex =
	case matchRegex ghc1_re l of {
	   Just (allocations:_:_:_:_:initialisation:_:mut:_:gc:_) ->
		got_run_result allocations initialisation mut gc Nothing
			Nothing Nothing Nothing Nothing;
	   Nothing -> 

	case matchRegex ghc2_re l of {
	   Just (allocations:_:_:_:_:_:initialisation:_:mut:_:gc:_) ->
		got_run_result allocations initialisation mut gc Nothing
			Nothing Nothing Nothing Nothing;

	    Nothing ->
	
	case matchRegex ghc3_re l of {
	   Just (allocations:_:_:_:_:gc_work':_:initialisation:_:mut:_:gc:_) ->
		got_run_result allocations initialisation mut gc (Just (read gc_work'))
			Nothing Nothing Nothing Nothing;

	    Nothing ->
	
	case matchRegex ghc4_re l of {
	   Just (allocations:_:_:_:_:gc_work':_:initialisation:_:mut:_:gc:_:is:mem_rs:mem_ws:cache_misses':_) ->
		got_run_result allocations initialisation mut gc (Just (read gc_work'))
			(Just (read is)) (Just (read mem_rs))
			(Just (read mem_ws)) (Just (read cache_misses'));

	    Nothing ->
	
	case matchRegex wrong_output l of {
	    Just ("stdout":_) -> 
		parse_run_time prog ls res (combineRunResult WrongStdout ex);
	    Just ("stderr":_) -> 
		parse_run_time prog ls res (combineRunResult WrongStderr ex);
	    Nothing ->
			
	case matchRegex wrong_exit_status l of {
	    Just (_wanted:got:_) -> 
		parse_run_time prog ls res (combineRunResult (Exit (read got)) ex);
	    Nothing -> 

	case matchRegex out_of_heap l of {
	    Just _ -> 
		parse_run_time prog ls res (combineRunResult OutOfHeap ex);
	    Nothing -> 

	case matchRegex out_of_stack l of {
	    Just _ -> 
		parse_run_time prog ls res (combineRunResult OutOfStack ex);
	    Nothing -> 
		parse_run_time prog ls res ex;

	}}}}}}}}
  where
  got_run_result allocations initialisation mut gc gc_work' instrs' mem_rs mem_ws cache_misses'
      = -- trace ("got_run_result: " ++ initialisation ++ ", " ++ mut ++ ", " ++ gc) $
	let 
	  read_mut = read mut
	  read_gc  = read gc
	  time = (read initialisation + read_mut + read_gc) :: Float 
	  res' = combine2Results res
	      		emptyResults{	run_time   = [time],
					mut_time   = [read_mut],
				  	gc_time    = [read_gc],
				  	gc_work    = gc_work',
				  	allocs     = Just (read allocations),
					instrs     = instrs',
				  	mem_reads  = mem_rs,
				  	mem_writes = mem_ws,
				  	cache_misses = cache_misses',
				  	run_status = Success 
				}
	in
	parse_run_time prog ls res' Success

combineRunResult :: Status -> Status -> Status
combineRunResult OutOfHeap  _           = OutOfHeap
combineRunResult _          OutOfHeap   = OutOfHeap
combineRunResult OutOfStack _           = OutOfStack
combineRunResult _          OutOfStack  = OutOfStack
combineRunResult (Exit e)   _           = Exit e
combineRunResult _          (Exit e)    = Exit e
combineRunResult exit       _		 = exit

parse_size :: String -> String -> [String] -> [(String, Results)]
parse_size _ _ [] = []
parse_size progName modName (l:ls) =
	case matchRegex size_re l of
	    Nothing -> parse_size progName modName ls
	    Just (text:datas:_bss:_) 
		 | progName == modName ->
			[(progName,emptyResults{binary_size = 
					      Just (read text + read datas),
				    compile_status = Success})]
		 | otherwise ->
			let ms  = Map.singleton modName (read text + read datas)
			in
			[(progName,emptyResults{module_size = ms})]

