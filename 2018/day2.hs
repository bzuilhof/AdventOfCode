module Day2 where

import Data.List
import Data.Char

inputData :: [String]
inputData = ["mphcuiszrnjzxwkbgdzqeoyxfa","mihcuisgrnjzxwkbgdtqeoylia","mphauisvrnjgxwkbgdtqeiylfa","mphcuisnrnjzxwkbgdgqeoylua","mphcuisurnjzxwkbgdtqeoilfi","mkhcuisvrnjzowkbgdteeoylfa","mphcoicvrnjzxwksgdtqeoylfa","mxhcuisvrndzxwkbgdtqeeylfa","dphcuisijnjzxwkbgdtqeoylfa","mihvuisvrqjzxwkbgdtqeoylfa","mphcuisrrnvzxwkbgdtqeodlfa","mphtuisdrnjzxskbgdtqeoylfa","mphcutmvsnjzxwkbgdtqeoylfa","mphcunsvrnjzswkggdtqeoylfa","mphcuisvrwjzxwkbpdtqeoylfr","mphcujsdrnjzxwkbgdtqeovlfa","mpfcuisvrdjzxwkbgdtteoylfa","mppcuisvrpjzxwkbgdtqeoywfa","mphcuisvrnjzxwkbfptqroylfa","mphcuisvrnjzxwkbgstoeoysfa","mphcufsvrnjzcwkbgdeqeoylfa","mphcuissrnjzxwkbgdkquoylfa","sphcuxsvrnjzxwkbgdtqioylfa","mphcuiivrhjzxwkbgdtqevylfa","echcuisvrnjzxwkbgltqeoylfa","mphcuisvrljexwkbvdtqeoylfa","mpjcuisvrnjzxwkhidtqeoylfa","mphcuisvrfjzmwkbgdtqeoylfl","mwhcuisvrnjzxwkbgdtqeoytfm","mphcuisvrsjzxwkbgdaqeoylfh","mohcuisvrnjzxwkbgdtqtoymfa","maycuisvrnjzxwkbgdtqboylfa","pphcuisvqnjzxwkbgdtqeoylfd","mprcuisvrnjtxwmbgdtqeoylfa","mfhcuisgrnjzxckbgdtqeoylfa","mphiubsvrnjzxwkbgdtqeoyufa","dphctisvrnjzxwkbgdtqeoylfk","mphcuisvrnjznwksgdtqeoyzfa","mpwcuisvrnjziwkbgdtqaoylfa","mphduzsvrnjznwkbgdtqeoylfa","mphccisvrnjzxwebgdtqeoylqa","xphcuisvrnjzxwkfvdtqeoylfa","mphcupsvrnjzxwkbgdtfeoylpa","mphcuisvrtjzjwkbgdtqeoylfe","mpbcuisvrnjzxwkbgdmieoylfa","mphcuisvrnjzxwkbgjtqetylaa","mphcuisvrnjzxwpbgdtgdoylfa","ophcufsvrqjzxwkbgdtqeoylfa","iphcuhsvrnjzxwkbgetqeoylfa","mphcuisvunjzxwwbgdtqeoylqa","mphcpisvrnjzowkbgdtveoylfa","mphcuisvrnjzxhkbgdtqeotlla","mphcuisvrnjzxwkbodtgeoylha","mphcuisvrjjzxwkbwdtqtoylfa","mphcwisvrnjnxwkbgjtqeoylfa","mplcuicqrnjzxwkbgdtqeoylfa","mphcuisvrnjzxydbgdtqeoylfn","ophckisvrnjzxwkbgdtqeozlfa","mphcuisvrkjzxwkbgdtteoblfa","yphcuisvrnjcxwkbggtqeoylfa","mphcuisvrnazxwfbqdtqeoylfa","mphcuisvrmjzxwkbgdtlwoylfa","mphctksvrnjzxwibgdtqeoylfa","mphcuisprnjzxlebgdtqeoylfa","mphcuisnrnjzxakbgdtueoylfa","mphcuiavrnjoxwtbgdtqeoylfa","nphcuisvrnjzxwkbgdtqzoylfk","mphcuisrrnjmxwkbgdtqdoylfa","mphcuisvrujzxwkvgdtqehylfa","mphcuisvrnfzxwkogdtqebylfa","mphcuisvrnjwdwkbgdtqeoyxfa","mphcuisvrntzxwkrgxtqeoylfa","mpzcuisvrnjzxwebgdtqeoylsa","aphcuikvrnjzxwwbgdtqeoylfa","mphcqisvrnjzxwkpgdtqeoelfa","mphcuusvrnjzxwkbgdtjeodlfa","mphcuisvrnjzewkbgdtteoylza","mphcuisvanjzxwkbgdtheoylfc","mphcjishrnjzxwkbgltqeoylfa","mpxcuislrnjzxwkbgdtqeoynfa","mphcuisvrnjjxwkbgdtmeoxlfa","mphcimsvrnjzxwkbsdtqeoylfa","mphcxisvcnjzxwjbgdtqeoylfa","mphcuisbrvjzxwkbgdtqeoymfa","mplcuisvrnjzxwkbgdtaenylfa","mphcuihvrnjzxwkygytqeoylfa","mphcbisvrnjzxhkbgdtqezylfa","mphcuisarnjzxwkbgatqeoylfv","mphcumsvrnjzxwkbgdrqebylfa","mlhcuisvrnwzxwkbgdtqeoylfx","mpkcuisvrkjzxwkbgdtqeoylfo","mphcuissrnjzxwkbgdtqmoylfc","mphcuiwvrnjuxwkfgdtqeoylfa","mphcuicvlnjzxwkbgdvqeoylfa","mphcuisvrvvzxwkbfdtqeoylfa","myhcuisvrnjpxwkbgntqeoylfa","mpocuisvrnjzxwtbgitqeoylfa","mphcuisvrnjzxwkbgdtwewyqfa","mphcuisvtnjzxwwbgdtqeoolfa","mphcuisvrnjzxgkbgdyqeoyyfa","mphcuisvrdjzxwkbgpyqeoylfa","bphcuisvrnjzxwkbgxtqefylfa","sphcuisvrdjzxwktgdtqeoylfa","mphcuvsvrnjmxwobgdtqeoylfa","mphcuisvrnjzxwkbsdtqeuylfb","mnhcmisvynjzxwkbgdtqeoylfa","mphckisvrnjzxwkhgdkqeoylfa","mpacuisvrnjzxwkbgdtqeoolaa","mpgcuisvrnjzxwkbzdtqeoynfa","mphcuisvrojzxwkbzdtqeoylga","mphcuisvknjfxwkbydtqeoylfa","mphcuistrnjzxwkbgdqqeuylfa","bpvcuiszrnjzxwkbgdtqeoylfa","mphcuxsvrnjzswkbgdtqeoelfa","mphcuisvbnjzxwlbgdtqeoylla","mphcuisvonczxwkbgktqeoylfa","mphcuisvrnkzxwvbgdtquoylfa","mphcuisvrnjzxokfgdtqeoylia","tphcuisvrnjzxwkbjdwqeoylfa","mihcuisvrnjzpwibgdtqeoylfa","mphcuisvrejzxwkbgdtqjuylfa","mprcuisvrnjixwkxgdtqeoylfa","mpqcuiszrnjzxwkbgdtqeodlfa","mphcuasvrnjzzakbgdtqeoylva","mphcuisvrnjzmwkbtdtqeoycfa","mphcuisvrnjzxwkbcdtqioylxa","mphckisvrnjzxwkbcdtqeoylfm","mphcuisvrnjuxwbogdtqeoylfa","mphcuisdrnjzxwkbldtqeoylfx","mphcuisvrnjoxwkbgdtqeyyyfa","mphcuicvqnjzxwkbgdtqeoylna","mpmcuisvrnjzxwkbgdtqktylfa","mphcuisvrnqzxwkggdtqeoykfa","mphcuisvryjzxwkbydtqejylfa","mphcugsvrnjzxwkbghtqeeylfa","rphcuusvrnjzxwkwgdtqeoylfa","zphwuiyvrnjzxwkbgdtqeoylfa","cphcuivvrnjzxwkbgdtqenylfa","mphcuisvrnjzxwkagotqevylfa","mprcuisvrcjzxwkbgdtqeoytfa","mphjugsvrnezxwkbgdtqeoylfa","mphcuisvryjzxwkbgltqeoylaa","mphcursvrnjzxfkbgdtqeoydfa","mphcuisvrcuzxwkbgdtqeoylfw","mphcuisvrijzxwkbgdtqeoelfh","xphcuisvenjzxjkbgdtqeoylfa","mphcuisvrnazxwkbgdeqeoylaa","mphcuisbrsjzxwkbgdtqeoygfa","mlhvuisvrnjzxwkbgdtqeoylfh","mphcuisvrnjzxukbgdtqeoyhfy","mpzcuilvrnjzawkbgdtqeoylfa","hphcuisjfnjzxwkbgdtqeoylfa","mahcuisvrnjzxwkegdtqeoylfi","mphcuixvrnjzcwkbgdtqetylfa","mphcuisvrnjzxwkdgdtqeoklfj","mlhcuisvrnjzxwkbgdteeoylka","mphcuifvrnjbxwkrgdtqeoylfa","mphcuasvrnjzzwkbgdtqeoylva","mphcuisvrnjzxwkboutqeoylba","mbhcuisvcnjzxwklgdtqeoylfa","mpbcuisvrnjzxgkbgdtqesylfa","mphcuisvrnjfswkbgdtqeoylfd","mphcuisvrnjzxwkbgdoweoysfa","uphcuisvrnjzrwkbgdtqelylfa","mphcuisvrnjzxwkbgdtqyoylsi","mpqcuiqvxnjzxwkbgdtqeoylfa","mphcuisorfjzxwkbgatqeoylfa","mphcuisvrntfxwkbzdtqeoylfa","mphcuisvrnrzxwkbgdtueoylfl","mphcuisvrnjzewkagdtyeoylfa","mpocuisdrnjzxwkbgdtqeozlfa","mphcuisvrnjjxwkbgdtoeoylfm","mphcuisvenjzxwkbgdtqwoylza","mpmcuisvrnjzxwkbgdtqeoxlfr","mphcuisvgnjhxwkbgdtqeoplfa","mphcuisvrnjzowkdgdtqeoyyfa","mphcuisqynjzxwkbgdtqeoylda","hphcuisvgnjzxwkbgdtbeoylfa","iphcuipvrnuzxwkbgdtqeoylfa","mphcuisvrnjzsikbpdtqeoylfa","mpwcuhsvrnjzxbkbgdtqeoylfa","mnhjuisvcnjzxwkbgdtqeoylfa","mphcudsvrnjzxwkbgdtqloilfa","mpncuiwvrwjzxwkbgdtqeoylfa","mphcuisvrnjgawkbgdtqeoylya","mphcuisvrnjzxwkbggtteoslfa","mphcuisvrnjzxwkbgdvqeoylpe","mphcuisvrnczxfkbgktqeoylfa","mphcuifvrnjzxwkbgdbmeoylfa","mphcuisvrnjytwkbgdtqeoylla","mphcuisvrnjzxwkbgdtjeoxlfn","mphjuisvrnjzxwkbghtqeoyffa","mphcuisvrnjzxkrbgdtqeoylaa","mphcbisvrnjzxwkbgttqeoylfs","mphkuksvbnjzxwkbgdtqeoylfa","nphcuidvrnjzxwhbgdtqeoylfa","mphguzsvrnjzxwkbgdaqeoylfa","mihcuisfrnjzxwkbgdtqhoylfa","mphcuisvrnrzxwpbgdtqesylfa","zphcuisvrnjzxwkbddtqeoylaa","mphcuigvmnjzxwkbgdtqeoylba","mjhcuisvrnjzxjkbgdtqeoylha","mphnuisvrnjznwkbgdtqnoylfa","mkhcuisvrnjcxwkbgdqqeoylfa","mphcuisvenjzxwbbqdtqeoylfa","qphcuisnrnjzawkbgdtqeoylfa","mphcuisvrdjzxwkbgdtqeoywca","mphcuzsvvnjzxwfbgdtqeoylfa","pphcuxsvrnjzxwkbgdtmeoylfa","mphiuvsvrnjzxlkbgdtqeoylfa","mphlqisvrnjzxkkbgdtqeoylfa","mmhcuisvrnjzxwkbgatqeoylea","mphduisrrnjoxwkbgdtqeoylfa","mphcuisvrnjnxwkvgdyqeoylfa","mphcuvsvrnjzxgkbgdtqeoylfz","mphcuisvryjzxwkbggtqkoylfa","iphcuisvrdjzxwkbgotqeoylfa","mphcuisvrnjzxwhbgdtqwoyofa","mphcorbvrnjzxwkbgdtqeoylfa","mghcuisvrnpzxykbgdtqeoylfa","mphauisvrnjnxwkbzdtqeoylfa","mphcgisvrnjzxwkwgdtqeoygfa","mphcuisvrnjzxwkggotqeoylba","mphcuesvrnjzxwkbgdwqebylfa","yphcuisvrnjzxwkbgdxqeoylja","ephyuisvrnjzywkbgdtqeoylfa","mfhcuisqrnjzxwkbgdlqeoylfa","mphkuisvrnjzxwkbertqeoylfa","mphcuusgrnjzxwkbggtqeoylfa","mphcuildrnjvxwkbgdtqeoylfa","mphcuiuvrnjzlwkbgwtqeoylfa","mppcuisvrljzxwkbgdtqeoylfw","mphcwiwvrnjzxwsbgdtqeoylfa","mphcubivrnjzxwkqgdtqeoylfa","mphcuisvrnjpxwkngdtqeoylpa","pchcuisvrgjzxwkbgdtqeoylfa","mphcuisvlnjzxwkbgdtmeoylfw","mphcuisvrnjzywkbgdvqeoylfj","mpzcuisvrnezxwktgdtqeoylfa","mphcuisvrnjbxwkbgzrqeoylfa","mphcuisvrnjzxwktgdtqeodtfa","jphcuiavrnjzxwkbgdtqeoylfv","mphcuisvrnjzxwkbddppeoylfa","mphcuissrkjzxwkbgxtqeoylfa","mphcuisvrhjzxwxbgdtqeoylxa","mphcvisvgnjjxwkbgdtqeoylfa","mphcuisprnjwxwtbgdtqeoylfa","mphcuissrnjzxqkbgdtqeoymfa","mphcuiabrnjzxokbgdtqeoylfa","mphcuisvrnczxwkbgmtpeoylfa"]

findTripOrDub :: String -> (Bool,Bool)
findTripOrDub s = (doubles, triples)
            where   counters = map length (group (sort s))
                    doubles = 2 `elem` counters
                    triples = 3 `elem` counters

rudiCheck :: [String] -> Int
rudiCheck xs =  length (filter fst tripsNDubs) * length (filter snd tripsNDubs)
                where tripsNDubs = map findTripOrDub xs

differsOneChar :: String -> String -> Bool -> Bool
differsOneChar [] [] oneDiff = oneDiff
differsOneChar (x:xs) (y:ys) oneDiff    | x == y = differsOneChar xs ys oneDiff
                                        | x /= y && not oneDiff = differsOneChar xs ys True
                                        | otherwise = False
differsOneCharFlip :: String -> Bool -> String -> Bool
differsOneCharFlip s1 b s2 = differsOneChar s1 s2 b


findS :: [String] -> String
findS [] = "Error"
findS (x:xs)    | length (filter (differsOneCharFlip x False) xs) == 1 = sameChars x (head (filter (differsOneCharFlip x False) xs))
                | otherwise = findS xs

sameChars :: String -> String -> String 
sameChars [] _ = ""
sameChars (x:xs) (y:ys) | x == y = x : sameChars xs ys
                        | otherwise = sameChars xs ys

resultP1 :: Int
resultP1 = rudiCheck inputData

resultP2 :: String
resultP2 = findS inputData