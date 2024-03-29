module Day1 where

import Prelude

import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Effect (Effect)
import Effect.Console (log)

masses :: Array Int
masses = [
120588,
137636,
114877,
118328,
97394,
58497,
139343,
80307,
125063,
70956,
119676,
76115,
91916,
64618,
82881,
57000,
141785,
73460,
68992,
125701,
97839,
137800,
111051,
104591,
114396,
60210,
80238,
112009,
70265,
140582,
58765,
96848,
130438,
55615,
53903,
109361,
129512,
75888,
93231,
54697,
125320,
53614,
87173,
71762,
147739,
131840,
123979,
54434,
121517,
113518,
83544,
124924,
76608,
130483,
149285,
134147,
111589,
88174,
136392,
94448,
139244,
54064,
85110,
102985,
95646,
54649,
129755,
135795,
119653,
147633,
108386,
143180,
126587,
119273,
130579,
56006,
83232,
99948,
147711,
83092,
99706,
98697,
143231,
94526,
53102,
86002,
71413,
111054,
147220,
136504,
59308,
61123,
148771,
113986,
55483,
94426,
62791,
100959,
63604,
112511
]

computeCombustible :: Int -> Int
computeCombustible masse
  | masse < 6 = 0
  | otherwise = do
      let combustible = masse / 3 - 2
      combustible + computeCombustible(combustible)

main :: Effect Unit
main = do
  let combustibles = sum (map computeCombustible masses)
  log (toStringAs decimal combustibles)
