module Nodes (startNode) where

import GameData

noCond :: Condition
noCond = noCondition

type Modifier = GameState -> GameState

modifyMany :: [Modifier] -> Modifier
modifyMany fs gs = foldl (flip ($)) gs fs

startNode :: Node
startNode = gameStart

-- JSON: gameStart
gameStart :: Node
gameStart = Node "Witaj! Wybierz jakim studentem jesteś:" [
    Choice "Programista" programmerNode noCond
      (modifyMany [
         \gs -> gs { ada = ada gs + 10 },
         \gs -> gs { c = c gs + 10 },
         \gs -> gs { oop = oop gs + 10 },
         \gs -> gs { matematyka = matematyka gs + 5 },
         \gs -> gs { charyzma = charyzma gs + 5 }
      ]),
    Choice "Matematyk" mathematicianNode noCond
      (modifyMany [
         \gs -> gs { ada = ada gs + 5 },
         \gs -> gs { c = c gs + 5 },
         \gs -> gs { oop = oop gs + 5 },
         \gs -> gs { matematyka = matematyka gs + 20 }
      ]),
    Choice "Leń" slobNode noCond
      (modifyMany [
         \gs -> gs { ada = ada gs + 5 },
         \gs -> gs { c = c gs + 5 },
         \gs -> gs { oop = oop gs + 5 },
         \gs -> gs { matematyka = matematyka gs + 5 },
         \gs -> gs { charyzma = charyzma gs + 20 }
      ])
  ]

-- JSON nodes: programmer, mathematician, slob
programmerNode, mathematicianNode, slobNode :: Node
programmerNode = Node
  "Gratulacje jesteś programistą. Wybrałeś szalony świat Ady, programowania obiektowego i nieskończonych segfaultów"
  [ Choice "Zacznij grę, obudź się!" wakeUpNode noCond
      (\gs -> gs { czas = 0, dzień = dzień gs + 1 }) ]
mathematicianNode = Node
  "Gratulacje umiesz matematykę! Czemu w takim razie studujesz informatykę?"
  [ Choice "Zacznij grę, obudź się!" wakeUpNode noCond
      (\gs -> gs { czas = 0, dzień = dzień gs + 1 }) ]
slobNode = Node
  "Po prostu jesteś leniwy"
  [ Choice "Zacznij grę, obudź się!" wakeUpNode noCond
      (\gs -> gs { czas = 0, dzień = dzień gs + 1 }) ]

-- JSON: wakeUp
wakeUpNode :: Node
wakeUpNode = Node
  "Budzisz się w złym nastroju. Co chcesz dzisiaj zrobić?"
  [ Choice "Idziesz na zajęcia" placGrunwaldzkiNode noCond id
  , Choice "Zostajesz w domu" homeNode noCond id
  ]

-- JSON: placGrunwaldzki
placGrunwaldzkiNode :: Node
placGrunwaldzkiNode = Node
  "Jesteś na Placu Grunwaldzkim, wybierz losową opcję"
  [ Choice "Zdarzenie losowe 1" metGebalaNode noCond id
  , Choice "Zdarzenie losowe 2" tramDerailNode noCond id
  , Choice "Zdarzenie losowe 3" meetFriendNode noCond id
  ]

metGebalaNode :: Node
metGebalaNode = Node "Spotykasz profesora Gębalę \n - Dzień doby! \n - Dzień dobry, Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque in rhoncus enim, aliquet feugiat leo. Nulla lacinia nisi et vehicula efficitur. Donec maximus iaculis felis, eu tempus ligula lobortis sed. Cras interdum massa lacus, eu tempus lorem luctus id. Vestibulum tincidunt consequat magna. Maecenas id enim risus. Sed nisl erat, tristique nec ullamcorper quis, imperdiet sit amet nulla. Nam rhoncus neque risus. Pellentesque molestie leo sed aliquet efficitur. Sed urna nunc, egestas vel dolor quis, blandit finibus mi. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Nam feugiat ullamcorper elit, non pharetra nibh lacinia ut. Nulla sem diam, tincidunt at blandit sit amet, condimentum tempor orci. Fusce urna diam, bibendum sit amet porttitor sit amet, vestibulum accumsan ligula. Donec dignissim, mi a tincidunt auctor, metus mi malesuada nunc, faucibus tempus sem leo sit amet mi. Etiam bibendum faucibus justo eu pellentesque."
  [ Choice "Wybierz na jakie zajęcia idziesz:" chooseClassNode noCond id ]

tramDerailNode :: Node
tramDerailNode = Node
  "Wykolejający się tramwaj przypomina ci, że mieszkasz we Wrocławiu. No nic, trzeba iść na zajęcia - pieszo..."
  [ Choice "Wybierz na jakie zajęcia idziesz:" chooseClassNode noCond id ]

meetFriendNode :: Node
meetFriendNode = Node
  "Spotykasz znajomego z liceum. Narzekacie sobie wspólnie na studia, jest przyjemnie... Przez chwilę, ale musisz iść na zajęcia"
  [ Choice "Wybierz na jakie zajęcia idziesz:" chooseClassNode noCond id ]

-- JSON: chooseClass
chooseClassNode :: Node
chooseClassNode = Node
  "Wygląda na to, że musisz podjąć poważną decyzję. Na jaki typ zajęć teraz pójdziesz?"
  [ Choice "Idź na ćwiczenia" goToExercisesNode
      (Condition (\gs -> czas gs < 3) "czas < 3")
      (\gs ->
        let bonus = if przygotowanie gs > 0 then (-1, matematyka gs * 2) else (0, matematyka gs)
        in gs { czas = czas gs + 1
              , przygotowanie = przygotowanie gs - fst bonus
              , punkty = punkty gs + snd bonus
              }
      ),
    Choice "Idź na laby" goToLabNode
      (Condition (\gs -> czas gs < 3 && dzień gs > 2) "czas < 3 && dzień > 2")
      (\gs -> gs { czas = czas gs + 1
                 , przygotowanie = przygotowanie gs - 1
                 , zadowolenie = zadowolenie gs - 1 }),
    Choice "Pora na wykład" goToLectureNode
      (Condition (\gs -> czas gs < 3) "czas < 3")
      (\gs -> gs { czas = czas gs + 1
                 , zadowolenie = zadowolenie gs - 1 }),
    Choice "Zmieniłem zdanie, zawijam na chatę" homeNode noCond id
  ]

-- JSON: goToExercises
goToExercisesNode :: Node
goToExercisesNode = Node ""
  [ Choice "Idź wybrać zajęcia" chooseClassNode noCond id ]

-- JSON: goToLab
goToLabNode :: Node
goToLabNode = Node
  "Czas przejść się na laboratoria. Jaki przedmiot dzisiaj?"
  [ Choice "Ada (laby)" adaLabNode noCond
      (\gs -> gs { obecnyPrzedmiot = "ada" }),
    Choice "C-lang (laby)" cLabNode noCond
      (\gs -> gs { obecnyPrzedmiot = "c" }),
    Choice "OOP (laby)" oopLabNode noCond
      (\gs -> gs { obecnyPrzedmiot = "oop" })
  ]

-- JSON: goToLecture
goToLectureNode :: Node
goToLectureNode = Node
  "Wchodzisz do sali wykładowej. O czym dzisiaj będzie wykład?"
  [ Choice "Ada (wykład)" adaLectureNode noCond
      (\gs -> gs { obecnyPrzedmiot = "ada" }),
    Choice "C (wykład)" cLectureNode noCond
      (\gs -> gs { obecnyPrzedmiot = "c" }),
    Choice "OOP (wykład)" oopLectureNode noCond
      (\gs -> gs { obecnyPrzedmiot = "oop" })
  ]

-- JSON: adaLab, cLab, oopLab
adaLabNode :: Node
adaLabNode = Node
  "W końcu udało ci się skompilować swój program. Jakie będą twoje dalsze działania?"
  [ Choice "Oddaj listę" duringLabNode noCond id ]

cLabNode :: Node
cLabNode = Node
  "C się nie linkuje i nie zapowiada się, żeby to się zmieniło"
  [ Choice "Oddaj listę" duringLabNode noCond id ]

oopLabNode :: Node
oopLabNode = Node
  "Design patterns to twoje drugie imię"
  [ Choice "Oddaj listę" duringLabNode noCond id ]

-- JSON: duringLab
duringLabNode :: Node
duringLabNode = Node
  "Podchodzisz do prowadzącego oddać listę"
  [ Choice "Jesteś przygotowany" handListInNode
      (Condition (\gs -> przygotowanie gs > 0) "przygotowanie > 0")
      (\gs ->
        let bonus = case obecnyPrzedmiot gs of
                      "ada" -> ada gs * 5
                      "c"   -> c gs * 5
                      "oop" -> oop gs * 5
                      _     -> 0
        in gs { punkty = punkty gs + bonus }
      ),
    Choice "Bluffujesz" bluffNode noCond
      (\gs ->
        if charyzma gs > 10 then gs { punkty = punkty gs + 10 } else gs
      )
  ]

-- JSON: lecture nodes
adaLectureNode, cLectureNode, oopLectureNode :: Node
adaLectureNode = Node "Wgłębiasz się w tajniki ADA CORE"
  [ Choice "Słuchaj dalej" duringLectureNode noCond id ]
cLectureNode = Node "Wgłębiasz się w tajniki C"
  [ Choice "Słuchaj dalej" duringLectureNode noCond id ]
oopLectureNode = Node "Wgłębiasz się w tajniki programowania obiektowego"
  [ Choice "Słuchaj dalej" duringLectureNode noCond id ]

-- JSON: handListIn, bluffAboutList
handListInNode :: Node
handListInNode = Node "Śpiewająco tłumaczysz wszystkie struktury, komentarze i funkcje"
  [ Choice "Zdane. Wybierasz następne zajęcia" chooseClassNode noCond
      (\gs -> gs { czas = czas gs + 1 }) ]

bluffNode :: Node
bluffNode = Node
  "No nic, nie masz pojęcia skąd wzięła się połowa tego kodu. Dostajesz 3.0 na szynach"
  [ Choice "Z głowy. Idziemy zaliczyć kolejny przedmiot" chooseClassNode noCond
      (\gs -> gs { czas = czas gs + 1 }) ]

duringLectureNode :: Node
duringLectureNode = Node
  "Po 15 minutach musisz podjąć trudną decyzję"
  [ Choice "Słuchasz wykładu" listenLectureNode noCond
      (\gs -> gs
        { zadowolenie = zadowolenie gs - 1
        , ada = if obecnyPrzedmiot gs == "ada" then ada gs + 1 else ada gs
        , c = if obecnyPrzedmiot gs == "c" then c gs + 1 else c gs
        , oop = if obecnyPrzedmiot gs == "oop" then oop gs + 1 else oop gs
        }),
    Choice "Rozmawiasz z ziomkiem" talkWithFriendsNode noCond
      (\gs -> gs { zadowolenie = zadowolenie gs + 1 })
  ]


listenLectureNode :: Node
listenLectureNode = Node
  "Nie jesteś pewien czy to było tego warte"
  [ Choice "Wybierz co zrobisz teraz" chooseClassNode noCond
      (\gs -> gs { czas = czas gs + 1 }) ]

talkWithFriendsNode :: Node
talkWithFriendsNode = Node
  "Koledzy opowiadają ci zabawne historie. Wykład mija ci szybciej"
  [ Choice "Wybierz co zrobisz teraz" chooseClassNode noCond
      (\gs -> gs { czas = czas gs + 1 }) ]

-- JSON: homeNode and follow-ups
-- (już w sustawie, ale z dodatkowymi follow‑ups)
prepareForLabNode, doNapNode, goDrinkBeerNode, goSleepNode :: Node

homeNode :: Node
homeNode = Node
  "Home sweeet home. Wynajęty dwuosobowy pokój w akademiku..."
  [ Choice "Przygotuj się na laby" prepareForLabNode
      (Condition (\gs -> czas gs < 5) "czas < 5")
      (\gs -> gs { czas = czas gs + 1, przygotowanie = przygotowanie gs + 1, zadowolenie = zadowolenie gs - 1 }),
    Choice "Idź na piwo" goDrinkBeerNode
      (Condition (\gs -> czas gs < 5) "czas < 5")
      (\gs -> gs { zadowolenie = zadowolenie gs + 3 }),
    Choice "Zrób drzemkę" doNapNode
      (Condition (\gs -> czas gs < 5) "czas < 5")
      (\gs -> gs { czas = czas gs + 1, zadowolenie = zadowolenie gs + 1 }),
    Choice "Zmieniamy zdanie, idziemy na zajęcia" placGrunwaldzkiNode
      (Condition (\gs -> czas gs < 2) "czas < 2") id
  ]

prepareForLabNode = Node
  "Spędzasz wieczór pisząc kod i żałując, że nie wybrałeś pójścia na piwo"
  [ Choice "Tak ci mija dzień" homeNode noCond id ]

doNapNode = Node
  "Ucinasz sobie drzemkę. Śni ci się, że nie zdałeś egzaminu z analizy. Budzisz się nieco bardziej wypoczęty"
  [ Choice "Trzeba zagospodarować resztę popołudnia" homeNode noCond id ]

goDrinkBeerNode = Node
  "Idziesz pić piwo. Nic dodać nic ująć"
  [ Choice "Wracasz zmęczony, już nic dzisiaj nie osiągniesz. Idziemy spać" goSleepNode noCond id ]

goSleepNode = Node
  "Dobranoc. Śpi się wybornie, ale kiedyś trzeba zacząć nowy dzień"
  [ Choice "Twój budzik dzwoni, czas wstać" wakeUpNode noCond
      (\gs -> gs { czas = 0, dzień = dzień gs + 1 }) ]
