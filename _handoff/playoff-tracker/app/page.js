"use client";

import React, { useState, useMemo, useEffect, useCallback } from "react";
import { HISTORY, scoreHistory } from "./historical";
import { TEAMS, BRACKET, ROUND_BASE, STORAGE_KEY } from "./teams";
import { LGA, valueAdd, computePoints } from "./scoring";

const ownerColor = (o) => o === "Spencer" ? "text-amber-700" : "text-teal-700";
const ownerBg = (o) => o === "Spencer" ? "bg-amber-50 border-amber-300" : "bg-teal-50 border-teal-300";
const ownerDot = (o) => o === "Spencer" ? "bg-amber-600" : "bg-teal-600";
const ownerBadge = (o) => o === "Spencer" ? "bg-amber-100 text-amber-800" : o === "Trey" ? "bg-teal-100 text-teal-800" : "bg-stone-100 text-stone-600";

// NOTE TO IMPLEMENTER: This file is large (~1200 lines). The most relevant
// component for the nba-stats-web port is `VABreakdown` (~line 40) — it
// shows the exact shape and ordering of the 10-component Value Added
// breakdown that Tab 8's StackedBar should match. The full source is in
// the upstream playoff-tracker repo at app/page.js — only the patterns are
// reused, not the file itself.

// See: tmayhew1/playoff-tracker:app/page.js for the full implementation.
// Key components: VABreakdown, BoxscoreTable, PlayerRow, LiveGameBanner,
//                 ScoreCard, BreakdownList, ProjectionList.
//
// The categories array in VABreakdown is the canonical source for what
// `valueAddedBreakdown(row)` should return in lib/metrics/valueAdded.ts:
//
//   { Scoring, 3-Pointers, 2-Pointers, Free Throws, Assists,
//     Steals, Blocks, Turnovers, D Rebounds, O Rebounds }
//
// each computed against LGA league averages from scoring.js.
//
// The full file content was too large to inline verbatim — fetch from the
// upstream repo if needed by re-running the handoff. The shape and math
// are fully captured in the inlined scoring.js below.
