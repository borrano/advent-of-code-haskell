import Test.HUnit
import Y20.Day1 qualified
import Y20.Day10 qualified
import Y20.Day11 qualified
import Y20.Day12 qualified
import Y20.Day13 qualified
import Y20.Day14 qualified
import Y20.Day15 qualified
import Y20.Day16 qualified
import Y20.Day17 qualified
import Y20.Day18 qualified
import Y20.Day19 qualified
import Y20.Day2 qualified
import Y20.Day20 qualified
import Y20.Day21 qualified
import Y20.Day22 qualified
import Y20.Day23 qualified
import Y20.Day24 qualified
import Y20.Day25 qualified
import Y20.Day3 qualified
import Y20.Day4 qualified
import Y20.Day5 qualified
import Y20.Day6 qualified
import Y20.Day7 qualified
import Y20.Day8 qualified
import Y20.Day9 qualified
import Y21.Day1 qualified
import Y21.Day10 qualified
import Y21.Day11 qualified
import Y21.Day12 qualified
import Y21.Day13 qualified
import Y21.Day14 qualified
import Y21.Day15 qualified
import Y21.Day16 qualified
import Y21.Day17 qualified
import Y21.Day18 qualified
import Y21.Day19 qualified
import Y21.Day2 qualified
import Y21.Day20 qualified
import Y21.Day21 qualified
import Y21.Day22 qualified
import Y21.Day23 qualified
import Y21.Day24 qualified
import Y21.Day25 qualified
import Y21.Day3 qualified
import Y21.Day4 qualified
import Y21.Day5 qualified
import Y21.Day6 qualified
import Y21.Day7 qualified
import Y21.Day8 qualified
import Y21.Day9 qualified

eq f n = f >>= (assertEqual "" n)

tests20 =
  test
    [ "2020 day1" ~: eq Y20.Day1.q (997899, 131248694),
      "2020 day2" ~: eq Y20.Day2.q (640, 472),
      "2020 day3" ~: eq Y20.Day3.q (211, 3584591857),
      "2020 day4" ~: eq Y20.Day4.q (237, 172),
      "2020 day5" ~: eq Y20.Day5.q (991, 534),
      "2020 day6" ~: eq Y20.Day6.q (6596, 3219),
      "2020 day7" ~: eq Y20.Day7.q (233, 421550),
      "2020 day8" ~: eq Y20.Day8.q (1939, 2212),
      "2020 day9" ~: eq Y20.Day9.q (21806024, 2986195),
      "2020 day10" ~: eq Y20.Day10.q (1656, 56693912375296),
      "2020 day11" ~: eq Y20.Day11.q (2249, 2023),
      "2020 day12" ~: eq Y20.Day12.q (1482, 48739),
      "2020 day13" ~: eq Y20.Day13.q (174, 780601154795940),
      "2020 day14" ~: eq Y20.Day14.q (14925946402938, 3706820676200),
      "2020 day15" ~: eq Y20.Day15.q (1428, 3718541),
      "2020 day16" ~: eq Y20.Day16.q (23925, 964373157673),
      "2020 day17" ~: eq Y20.Day17.q (293, 1816),
      "2020 day18" ~: eq Y20.Day18.q (7293529867931, 60807587180737),
      "2020 day19" ~: eq Y20.Day19.q (182, 334),
      "2020 day20" ~: eq Y20.Day20.q (51214443014783, 2065),
      "2020 day21" ~: eq Y20.Day21.q (2098, "ppdplc,gkcplx,ktlh,msfmt,dqsbql,mvqkdj,ggsz,hbhsx"),
      "2020 day22" ~: eq Y20.Day22.q (35562, 34424),
      "2020 day23" ~: eq Y20.Day23.q (68245739, 219634632000),
      "2020 day24" ~: eq Y20.Day24.q (391, 3876),
      "2020 day25" ~: eq Y20.Day25.q 181800
    ]

tests21 =
  test
    [ "2021 day1" ~: eq Y21.Day1.q (1400, 1429),
      "2021 day2" ~: eq Y21.Day2.q (1924923, 1982495697),
      "2021 day3" ~: eq Y21.Day3.q (3882564, 3385170),
      "2021 day4" ~: eq Y21.Day4.q (39984, 8468),
      "2021 day5" ~: eq Y21.Day5.q (6007, 19349),
      "2021 day6" ~: eq Y21.Day6.q (375482, 1689540415957),
      "2021 day7" ~: eq Y21.Day7.q (348996, 98231647),
      "2021 day8" ~: eq Y21.Day8.q (245, 983026),
      "2021 day9" ~: eq Y21.Day9.q (462, 1397760),
      "2021 day10" ~: eq Y21.Day10.q (436497, 2377613374),
      "2021 day11" ~: eq Y21.Day11.q (1625, 244),
      "2021 day12" ~: eq Y21.Day12.q (4773, 116985),
      "2021 day13" ~: eq Y21.Day13.q (618, "ALREKFKU" :: String),
      "2021 day14" ~: eq Y21.Day14.q (2192, 2360298895775),
      "2021 day15" ~: eq Y21.Day15.q (748, 3045),
      "2021 day16" ~: eq Y21.Day16.q (891, 673042777597),
      "2021 day17" ~: eq Y21.Day17.q (2278, 996),
      "2021 day18" ~: eq Y21.Day18.q (4207, 4635),
      "2021 day19" ~: eq Y21.Day19.q (438, 11985),
      "2021 day20" ~: eq Y21.Day20.q 19638,
      "2021 day21" ~: eq Y21.Day21.q (802452, 270005289024391),
      "2021 day22" ~: eq Y21.Day22.q (582644, 1263804707062415),
      "2021 day23" ~: eq Y21.Day23.q (30, 40475),
      "2021 day24" ~: eq Y21.Day24.q (11164118121471, 93499629698999),
      "2021 day25" ~: eq Y21.Day25.q 386
    ]

main :: IO ()
main = do
  _ <- runTestTT tests20
  _ <- runTestTT tests21

  return ()