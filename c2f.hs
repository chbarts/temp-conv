import System.Environment
import System.Exit
import System.IO
import Text.Read

main = getArgs >>= parse >> exit

parse []       = usage
parse (n:nums) = firstarg n >> mapM_ doconv nums

args "-h"        = usage   >> exit
args "--help"    = usage   >> exit
args "-v"        = version >> exit
args "--version" = version >> exit
args str         = hPutStrLn stderr ("Invalid argument " ++ str) >> abend

usage = do
            putStrLn "c2f [-hv] [--help|--version] [nums...]"
            putStrLn "Prints the Fahrenheit conversions of inputs in Celsius"

version = putStrLn "c2f version 1.0"

exit  = exitWith ExitSuccess
abend = exitWith (ExitFailure 1)

firstarg n = case (readMaybe n) of
                 Just x  -> putStrLn (show (conv x))
                 Nothing -> args n

doconv str = case (readMaybe str) of
                 Just x  -> putStrLn (show (conv x))
                 Nothing -> hPutStrLn stderr ("Invald number " ++ str)

mix    a b x     = a*(1 - x) + b*x
linear a b x     = (x - a)/(b - a)
remap  a b c d x = mix c d (linear a b x)

conv c = remap 0 100 32 212 c
