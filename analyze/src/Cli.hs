module Cli
  ( Options(..)
  , getOptions
  ) where

import           Options.Applicative

data Options =
  Options {
    inputFilePath  ∷ FilePath,
    tikaJarPath    ∷ Maybe FilePath,
    javaExecutable ∷ String
  } deriving (Show)


getOptions ∷ IO Options
getOptions = execParser cli


cli ∷ ParserInfo Options
cli = info (optionsP <**> helper)
   ( fullDesc
  <> progDesc "Extracts Oregon session law metadata."
  <> header   "A command line app, which pulls in an Oregon session law\
              \ in PDF format and extracts this metadata to JSON." )


optionsP ∷ Parser Options
optionsP =
  Options
  <$> argument str
     ( metavar "FILENAME"
    <> help    "Path to .PFD-file" )
  <*> optional
     ( strOption
        ( short   't'
       <> long    "tika-jar"
       <> metavar "PATH_TO_JAR"
       <> help    "Path to Tika's .JAR-file" ) )
  <*> strOption
     ( short   'j'
    <> long    "java-executable"
    <> value   "java"
    <> metavar "JAVA_EXECUTABLE"
    <> help    "Name of Java exacutable" )
