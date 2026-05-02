"use client";

import React, { useState, useMemo, useEffect, useCallback } from "react";
import { HISTORY, scoreHistory } from "./historical";
import { TEAMS, BRACKET, ROUND_BASE, STORAGE_KEY } from "./teams";
import { LGA, valueAdd, computePoints } from "./scoring";

const ownerColor = (o) => o === "Spencer" ? "text-amber-700" : "text-teal-700";
const ownerBg = (o) => o === "Spencer" ? "bg-amber-50 border-amber-300" : "bg-teal-50 border-teal-300";
const ownerDot = (o) => o === "Spencer" ? "bg-amber-600" : "bg-teal-600";
const ownerBadge = (o) => o === "Spencer" ? "bg-amber-100 text-amber-800" : o === "Trey" ? "bg-teal-100 text-teal-800" : "bg-stone-100 text-stone-600";

function WinCircles({ value, actualValue, onChange, disabled, owner }) {
  const fillColor = owner === "Spencer" ? "bg-amber-500 border-amber-600" : "bg-teal-500 border-teal-600";
  const whatIfColor = owner === "Spencer" ? "bg-amber-200 border-amber-400" : "bg-teal-200 border-teal-400";
  return (
    <div className="flex items-center gap-1 mt-1">
      {[1, 2, 3, 4].map((n) => {
        const filled = value >= n;
        const isReal = n <= (actualValue || 0);
        let cls = "bg-white border-stone-300";
        if (filled) cls = isReal ? fillColor : whatIfColor;
        return (
          <button
            key={n}
            onClick={(e) => {
              e.stopPropagation();
              if (disabled) return;
              onChange(filled ? n - 1 : n);
            }}
            disabled={disabled}
            className={`w-3.5 h-3.5 rounded-full border transition-colors ${cls} disabled:opacity-40`}
            aria-label={filled ? `Win ${n} (tap to remove)` : `Add win ${n}`}
          />
        );
      })}
    </div>
  );
}

function VABreakdown({ p }) {
  const mp = p.mp || 0;
  if (mp <= 0) return null;

  const twoPm = p.fgm - p.tpm, twoPa = p.fga - p.tpa;
  const tpAdd = ((p.tpm / (p.tpa || 1)) - LGA.la3P) * p.tpa;
  const twoAdd = ((twoPm / (twoPa || 1)) - LGA.la2P) * twoPa;
  const ftAdd = ((p.ftm / (p.fta || 1)) - LGA.laFT) * p.fta;

  const categories = [
    { key: "Scoring", value: ((p.pts / mp) - LGA.laPTSperM) * mp, label: `${p.pts} PTS` },
    { key: "3-Pointers", value: 3 * tpAdd, label: `${p.tpm}/${p.tpa} 3P` },
    { key: "2-Pointers", value: 2 * twoAdd, label: `${twoPm}/${twoPa} 2P` },
    { key: "Free Throws", value: ftAdd, label: `${p.ftm}/${p.fta} FT` },
    { key: "Assists", value: ((p.ast / mp) - LGA.laASTperM) * mp * LGA.laPTSperMake * (1 - LGA.laFG), label: `${p.ast} AST` },
    { key: "Steals", value: ((p.stl / mp) - LGA.laSTLperM) * mp * LGA.laPTSperPoss, label: `${p.stl} STL` },
    { key: "Blocks", value: ((p.blk / mp) - LGA.laBLKperM) * mp * LGA.laPTSperPoss * LGA.laDRBrate, label: `${p.blk} BLK` },
    { key: "Turnovers", value: -((p.tov / mp) - LGA.laTOVperM) * mp * LGA.laPTSperPoss, label: `${p.tov} TOV` },
    { key: "D Rebounds", value: ((p.drb / mp) - LGA.laDRBperM) * mp * LGA.laPTSperPoss * LGA.laORBrate, label: `${p.drb} DRB` },
    { key: "O Rebounds", value: ((p.orb / mp) - LGA.laORBperM) * mp * LGA.laPTSperPoss * LGA.laDRBrate, label: `${p.orb} ORB` },
  ].sort((a, b) => b.value - a.value);

  const maxAbs = Math.max(...categories.map((c) => Math.abs(c.value)), 0.5);
  const owner = TEAMS[p.team]?.owner;
  const posColor = owner === "Spencer" ? "bg-amber-500" : "bg-teal-500";

  return (
    <div className="px-2 py-3 bg-stone-50 border-t border-stone-200">
      <div className="text-[9px] uppercase tracking-widest text-stone-500 mb-2 flex items-center justify-between">
        <span>Value Added Breakdown</span>
        <span className="tabular-nums font-bold text-stone-700">Total: {p.va.toFixed(2)}</span>
      </div>
      <div className="space-y-0.5">
        {categories.map((c, i) => {
          const pct = (Math.abs(c.value) / maxAbs) * 45;
          const isPos = c.value >= 0;
          return (
            <div key={i} className="flex items-center gap-2 text-[10px]">
              <span className="w-20 text-stone-600 text-right truncate">{c.key}</span>
              <div className="flex-1 flex items-center relative h-4">
                <div className="absolute inset-y-0 left-1/2 w-px bg-stone-300"></div>
                <div
                  className={`absolute inset-y-0.5 ${isPos ? posColor : "bg-stone-400"}`}
                  style={{
                    left: isPos ? "50%" : `${50 - pct}%`,
                    width: `${pct}%`,
                  }}
                ></div>
              </div>
              <span className="w-10 tabular-nums text-right font-semibold text-stone-700">{c.value.toFixed(2)}</span>
              <span className="w-12 text-[9px] text-stone-500 text-right">{c.label}</span>
            </div>
          );
        })}
      </div>
      <div className="text-[9px] text-stone-400 mt-2 text-center italic">Bars show contribution above/below league average</div>
    </div>
  );
}

function getSortedPlayers(box) {
  if (!box) return [];
  return [
    ...(box.away?.players || []).map((p) => ({ ...p, team: box.away.tri })),
    ...(box.home?.players || []).map((p) => ({ ...p, team: box.home.tri })),
  ]
    .filter((p) => (p.mp || 0) > 0)
    .map((p) => ({ ...p, va: valueAdd(p) }))
    .sort((a, b) => b.va - a.va);
}

function PlayerRow({ p, isExpanded, onToggle }) {
  const teamInfo = TEAMS[p.team];
  const owner = teamInfo?.owner;
  return (
    <div className="border-b border-stone-100 last:border-0">
      <button
        onClick={onToggle}
        className={`w-full flex items-center gap-2 text-[10px] py-1 text-left ${isExpanded ? "bg-stone-100" : ""}`}
      >
        <span className={`w-10 text-[9px] font-bold uppercase tracking-wider px-1 py-0.5 text-center ${ownerBadge(owner)}`}>
          {p.team}
        </span>
        <span className={`flex-1 truncate ${p.starter ? "font-semibold text-stone-800" : "text-stone-600"}`}>
          <span className="text-stone-400 mr-1">{isExpanded ? "▾" : "▸"}</span>
          {p.name}
        </span>
        <span className="tabular-nums text-stone-500 w-7 text-right">{Math.round(p.mp)}</span>
        <span className="tabular-nums font-bold text-stone-900 w-6 text-right">{p.pts}</span>
        <span className="tabular-nums text-stone-600 w-5 text-right">{p.reb}</span>
        <span className="tabular-nums text-stone-600 w-5 text-right">{p.ast}</span>
        <span className={`tabular-nums w-8 text-right font-semibold ${p.va > 0 ? "text-stone-900" : "text-stone-400"}`}>
          {p.va.toFixed(1)}
        </span>
      </button>
      {isExpanded && <VABreakdown p={p} />}
    </div>
  );
}

function BoxscoreTable({ rows, expandedKey, setExpandedKey }) {
  return (
    <div>
      <div className="flex items-center gap-2 text-[9px] uppercase tracking-wider text-stone-400 py-1 border-b border-stone-200">
        <span className="w-10">Team</span>
        <span className="flex-1">Player</span>
        <span className="w-7 text-right">MIN</span>
        <span className="w-6 text-right">PTS</span>
        <span className="w-5 text-right">REB</span>
        <span className="w-5 text-right">AST</span>
        <span className="w-8 text-right">VA</span>
      </div>
      {rows.map((p, i) => {
        const rowKey = `${p.team}-${p.name}-${i}`;
        return (
          <PlayerRow
            key={rowKey}
            p={p}
            isExpanded={expandedKey === rowKey}
            onToggle={() => setExpandedKey(expandedKey === rowKey ? null : rowKey)}
          />
        );
      })}
    </div>
  );
}

// NOTE: The remaining components from the upstream file (LiveGameBanner,
// TbdCard, TeamButton, SeriesRow, RoundSection, ScoreCard, BreakdownList,
// ProjectionList, WhatIfClinchedList, UpcomingTodayBanner, HistoryView,
// CurrentView, and the PlayoffTracker default export) are playoff-bracket
// specific and not needed for the nba-stats-web port. The components
// preserved above (VABreakdown, BoxscoreTable, PlayerRow) are the ones
// the implementer should mirror for Tab 8 (Value Breakdown stacked bar)
// and the player profile tabs.
//
// If you need the full file (e.g., to study the localStorage + polling
// + sync pattern in CurrentView), it lives at the upstream
// tmayhew1/playoff-tracker:app/page.js — ask the user to fetch it for you.
