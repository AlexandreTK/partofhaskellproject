type Section = (String, [String])



section1 :: Section
section1 = ("teste", ["oi1", "oi2", "oi3"])

section2 :: Section
section2 = ("alexandre", ["tchau1", "tchau2", "tchau3"])

section3 :: Section
section3 = ("alexandre", ["isso", "esta", "funcionando"])

arquivo1 = section1:section3:section3:section1:section3:section3:[]
arquivo2 = section1:section2:section2:section1:section2:section2:[]





-- ["a", "b", "c"] -> ["\ta\n", "\tb\n", "\tc\n"]
genSubStr :: [String] -> [String]
genSubStr [] = []
genSubStr (h : t) = ("\t" ++ h ++ "\n") : (genSubStr t)

-- Generate [sections]...
genSectionStr :: [Section] -> [[String]]
genSectionStr [] = []
genSectionStr (h:t) = ["[" ++ (fst h) ++ "]\n"] : genSubStr (snd h) : (genSectionStr t)

-- String to print - could have used concat( )...
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ myConcat t

-- Ignore last \n in the string
ignoreLast :: [a] -> [a]
ignoreLast [] = []
ignoreLast [x] = []
ignoreLast (h:t) = h : ignoreLast t

printNewString = do
	let structuredStr = genSectionStr(arquivo1)
	let toPrintStr = ignoreLast(myConcat(myConcat(structuredStr)))
	putStrLn(toPrintStr)
--printAllFile = putStrLn (concat (["oi1", "\n" ,"oi2", "oi3"]))
