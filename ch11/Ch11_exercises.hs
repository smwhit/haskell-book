import Data.List

data OperatingSystem = 
      GnuPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os:: OperatingSystem,
                 lang:: ProgLang}
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {
    os = Mac,
    lang = Haskell
}

feelingWizardly = 
    Programmer { 
        lang = Agda,
        os = GnuPlusLinux
    }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer {
    lang = lang', os = os'
}| os' <- allOperatingSystems, lang' <- allLanguages ]

dd = length $ Data.List.nub allProgrammers

data Weekday =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday

f Friday = "Miller Time"