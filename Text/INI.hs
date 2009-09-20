--------------------------------------------------------------------------------
-- |
-- Module      : Text.INI
-- Copyright   : Copyright (C) 2009 by Jonas Kramer
-- License     : GNU General Public License (GPL)
--
-- Maintainer  : jkramer@nex.scrapping.cc
-- Stability   : stable
-- Portability : portable
--
-- Functions for parsing and querying INI formatted text.
--
--------------------------------------------------------------------------------

module Text.INI where

    import System.IO
    import Data.List
    import Data.Char
    import Data.Maybe


    type INI = [(String, [(String, String)])]


    -- | 'loadINI' takes a path to a file, loads the file and parses it,
    -- assuming it's INI formatted. May throw an IO error when 'readFile'
    -- fails.
    loadINI :: FilePath -> IO INI
    loadINI path = parseINI `fmap` readFile path


    -- | Parse the given string - assuming its INI formatted - and return an
    -- INI structure.
    parseINI :: String -> INI
    parseINI =
        fold . parse "" . filter ((/=) 0 . length) . map trim . lines

        where
            dropSpaces = dropWhile isSpace
            trim = reverse . dropSpaces . reverse . dropSpaces

            -- Parse a list if INI lines recursively.
            parse _ [] = []
            parse section (line:rest) =
                if ("[" `isPrefixOf` line) && ("]" `isSuffixOf` line)
                    then parse (trim $ sectionName line) rest
                    else (section, keyValuePair line) : parse section rest

                where
                    -- Extract the section name from a "[foo]"-like string.
                    sectionName = drop 1 . reverse . drop 1 . reverse

                    -- Extract a key/value pair from a "foo = bar"-like string.
                    keyValuePair line =
                        (key, value)
                        where
                            key = trim $ fst rawTuple
                            value = trim $ drop 1 $ snd rawTuple
                            rawTuple = span ((/=) '=') line

            fold triplets =
                map (\ section -> (section, pairs section)) sections
                where
                    sections = nub $ map fst triplets
                    pairs section = map snd $ filter ((==) section . fst) triplets


    -- | 'sections' returns a list of section names of an INI structure. May
    -- include an empty string if there are global settings that do not have a
    -- section declaration.
    sections :: INI -> [String]
    sections = map fst


    -- | 'section' maybe returns the content of a given section in the INI
    -- structure, or nothing if the section isn't found.
    section :: String -> INI -> Maybe [(String, String)]
    section = lookup


    -- | Like 'section', but returns an empty list if the section is not found.
    section' :: String -> INI -> [(String, String)]
    section' name ini =
        case (section name ini) of
            Nothing -> []
            Just content -> content


    -- | Lookup a value within the INI structure. If the path contains a dot,
    -- the part before the dot is used as the name of section to lookup the
    -- value in, otherwise the value is searched in global space (not within
    -- a section).
    value :: String -> INI -> Maybe String
    value path ini =
        section sectionName ini >>= lookup key
        where
            isPath = '.' `elem` path

            sectionName =
                if isPath
                    then takeWhile ((/=) '.') path
                    else ""

            key =
                if isPath
                    then drop 1 $ dropWhile ((/=) '.') path
                    else path
