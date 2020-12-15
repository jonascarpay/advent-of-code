{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AOC2020 where

import AOC2020.Day01
import AOC2020.Day02
import AOC2020.Day03
import AOC2020.Day04
import AOC2020.Day05
import AOC2020.Day06
import AOC2020.Day07
import AOC2020.Day08
import AOC2020.Day09
import AOC2020.Day10
import AOC2020.Day11
import AOC2020.Day12
import AOC2020.Day13
import AOC2020.Day14
import AOC2020.Day15
import Test.Hspec

aoc2020 :: Spec
aoc2020 = do
  describe "day 1" day1
  describe "day 2" day2
  describe "day 3" day3
  describe "day 4" day4
  describe "day 5" day5
  describe "day 6" day6
  describe "day 7" day7
  describe "day 8" day8
  describe "day 9" day9
  describe "day 10" day10
  describe "day 11" day11
  describe "day 12" day12
  describe "day 13" day13
  describe "day 14" day14
  describe "day 15" day15
