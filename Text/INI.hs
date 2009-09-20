



module Text.INI where

	import System.IO
	import Data.List
	import Data.Char
	import Data.Maybe


	type INI = [(String, [(String, String)])]


	loadINI path = readFile path >>= return . parseINI


	parseINI =
		fold . parse "" . filter ((/=) 0 . length) . map (trim) . lines

		where
			dropSpaces = dropWhile (isSpace)
			trim = reverse . dropSpaces . reverse . dropSpaces

			-- Parse a list if INI lines recursively.
			parse _ [] = []
			parse section (line:rest) =
				if ("[" `isPrefixOf` line) && ("]" `isSuffixOf` line)
					then parse (trim $ sectionName $ line) rest
					else (section, keyValuePair line) : (parse section rest)

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
					sections = nub $ map (fst) triplets
					pairs section = map (snd) $ filter ((==) section . fst) triplets


	sections :: INI -> [String]
	sections = map fst


	section :: String -> INI -> Maybe [(String, String)]
	section = lookup


	section' :: String -> INI -> [(String, String)]
	section' name ini =
		case (section name ini) of
			Nothing -> []
			Just content -> content


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
