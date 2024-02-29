-- understanding the format of SWIFT transactions since it is in the news

-- https://www.sepaforcorporates.com/swift-for-corporates/read-swift-message-structure/
import System.Console.Pretty
  ( Color (..),
    Style (..),
    bgColor,
    color,
    style,
  )
import GHC.Base (divInt)
import GHC.Natural (Natural)
import Data.Maybe (isNothing, fromJust)
type InstituteCode = (Char,Char,Char,Char)
type CountryCode = (Char,Char)
type LocationCode = (Char,Char)
type BranchCode = (Char,Char,Char)

prettyInst :: InstituteCode -> String
prettyInst (c1,c2,c3,c4) = color Green [c1,c2,c3,c4]
prettyCountry :: CountryCode -> String
prettyCountry (c1,c2) = color Red [c1,c2]
prettyLoc :: LocationCode -> String
prettyLoc (c1,c2) = color Blue [c1,c2]
prettyBranch :: BranchCode -> String
prettyBranch (c1,c2,c3) = color White [c1,c2,c3]

data SwiftCODE = SwiftCODE {instituteCode :: InstituteCode, countryCode :: CountryCode, locationCode :: LocationCode, branchCode :: Maybe BranchCode}

orDefault :: (a->b) -> b -> Maybe a -> (b,Bool)
orDefault f _ (Just x) = (f x,False)
orDefault _ defaulter Nothing = (defaulter,True)

dummySwiftCODE :: SwiftCODE
dummySwiftCODE = SwiftCODE {instituteCode = ('B','O','F','A'), countryCode = ('U','S'), locationCode = ('M','D'), branchCode = Nothing}

instance Show SwiftCODE where
    show x = prettyInst (instituteCode x) ++ prettyCountry (countryCode x) ++ prettyLoc (locationCode x) ++ fst (orDefault prettyBranch "" (branchCode x))
codeReadSwift :: String -> Maybe SwiftCODE
codeReadSwift [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11] = Just SwiftCODE {instituteCode = (x1,x2,x3,x4), countryCode = (x5,x6), locationCode = (x7,x8), branchCode = Just (x9,x10,x11)}
codeReadSwift [x1,x2,x3,x4,x5,x6,x7,x8] = Just SwiftCODE {instituteCode = (x1,x2,x3,x4), countryCode = (x5,x6), locationCode = (x7,x8), branchCode = Nothing}
codeReadSwift _ = Nothing

logicalTerminalAddress :: Char -> SwiftCODE -> String
logicalTerminalAddress c x = prettyInst (instituteCode x) ++ prettyCountry (countryCode x) ++ prettyLoc (locationCode x) ++ c:fst (orDefault prettyBranch "" (branchCode x))

type ApplicationID = String

data Digit = ZERO | ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE deriving (Enum,Show)

type ServiceID = [Digit]
prettyServiceID :: Color -> ServiceID -> String
prettyServiceID _ [] = ""
prettyServiceID col (x:xs) = color col $ show (fromEnum x) ++ prettyServiceID col xs
codeReadServiceId :: String -> ServiceID
codeReadServiceId = map (\x -> toEnum $ (read (x:"") :: Int) `mod` 10)

type SessionNumber = Natural
type SequenceNumber = Natural

data BasicHeaderBlock = BasicHeaderBlock {appId :: ApplicationID , serviceID :: ServiceID, swiftCode :: SwiftCODE , 
                                          sessNum :: SessionNumber, seqNum :: SequenceNumber}
instance Show BasicHeaderBlock where
    show x = let p1 = color Blue (appId x) 
                 p2 = prettyServiceID Blue (serviceID x)
                 p3 = logicalTerminalAddress 'Z' (swiftCode x)
                 p4 = color Green (show (sessNum x))
                 p5 = color Red (show (seqNum x)) in
      "{1:" ++ p1 ++ p2 ++ p3 ++ p4 ++ p5 ++ "}"

type MT = (Digit,Digit,Digit)
prettyMT :: MT -> String
prettyMT (d1,d2,d3) = color Red $ show $ (100*fromEnum d1)+(10*fromEnum d2)+fromEnum d3
codeReadMT :: String -> Maybe MT
codeReadMT x = let y = (read x :: Int) in if y<0 || y>999 then Nothing else Just (toEnum $ divInt y 100 `mod` 10,toEnum $ divInt y 10 `mod` 10,toEnum $ y `mod` 10)

data Priority = UrgentP | NormalP | SystemP
instance Show Priority where
  show UrgentP = "U"
  show NormalP = "N"
  show SystemP = "S"
data ApplicationHeaderBlock = ApplicationHeaderBlock{areSender :: Bool, messageType :: MT, recipientSwiftCode :: SwiftCODE, priority :: Priority, 
                                                      deliveryMonitoring :: Maybe String, obsolecencePeriod :: Maybe String}
instance Show ApplicationHeaderBlock where
  show x = let p1 = (if areSender x then "I" else "O")
               p2 = prettyMT (messageType x)
               p3 = logicalTerminalAddress 'X' (recipientSwiftCode x)
               p4 = color Green $ show $ priority x
               p5 = color Blue ( fst (orDefault id "" (deliveryMonitoring x)) ++ fst (orDefault id "" (obsolecencePeriod x))) in
    "{2:" ++ p1 ++ p2 ++ p3 ++ p4 ++ p5 ++ "}"

main :: IO ()
main = do
    putStrLn "Enter Application ID such as F"
    appID <- getLine
    putStrLn "Enter Service ID (01 for FIN, 21 for acknowledgement of negative acknowledgement) as some string of decimal digits"
    serviceID <- codeReadServiceId <$> getLine
    putStrLn $ "Enter SWIFT CODE e.g. " ++ show dummySwiftCODE ++ "and possibly 3 more characters for a branch code"
    (swiftCode,defaultUsed) <- orDefault id dummySwiftCODE . codeReadSwift <$> getLine
    putStrLn "Enter session number (a natural number)"
    sessNum <- readLn
    putStrLn "Enter sequence number (a natural number)"
    seqNum <- readLn
    if not defaultUsed then print BasicHeaderBlock{appId = appID, serviceID=serviceID,swiftCode=swiftCode,sessNum=sessNum,seqNum=seqNum} else print "Invalid SwiftCODE"