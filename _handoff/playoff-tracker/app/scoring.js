// Scoring logic: points computation and Value Added (VA) player stat.

import { TEAMS, BRACKET, ROUND_BASE, ROUND_LABEL } from "./teams";

// League averages for 2025-26 season, used to compute Value Added
export const LGA = {
  la3P: 0.359686938670772,
  la2P: 0.548356161904934,
  laFT: 0.788506191950464,
  laFG: 0.470335430881713,
  laPTSperM: 0.408655965562845,
  laASTperM: 0.0827805842301779,
  laSTLperM: 0.032258064516129,
  laBLKperM: 0.0143884892086331,
  laTOVperM: 0.0516272842803455,
  laDRBperM: 0.121786420566908,
  laORBperM: 0.0384615384615385,
  laPTSperMake: 2.31624664395461,
  laPTSperPoss: 1.01391216652376,
  laDRBrate: 0.738162582316744,
  laORBrate: 0.261837417683256,
};

export function valueAdd(p) {
  const { mp, pts, ast, stl, blk, tov, drb, orb, tpm, tpa, fgm, fga, ftm, fta } = p;
  if (!mp || mp <= 0) return 0;
  const twoPm = fgm - tpm, twoPa = fga - tpa;
  const tpAdd = ((tpm / (tpa || 1)) - LGA.la3P) * tpa;
  const twoAdd = ((twoPm / (twoPa || 1)) - LGA.la2P) * twoPa;
  const ftAdd = ((ftm / (fta || 1)) - LGA.laFT) * fta;
  const volume = ((pts / mp) - LGA.laPTSperM) * mp;
  const efficiency = 3 * tpAdd + 2 * twoAdd + ftAdd;
  const astVal = ((ast / mp) - LGA.laASTperM) * mp * LGA.laPTSperMake * (1 - LGA.laFG);
  const stlVal = ((stl / mp) - LGA.laSTLperM) * mp * LGA.laPTSperPoss;
  const blkVal = ((blk / mp) - LGA.laBLKperM) * mp * LGA.laPTSperPoss * LGA.laDRBrate;
  const tovVal = -((tov / mp) - LGA.laTOVperM) * mp * LGA.laPTSperPoss;
  const drbVal = ((drb / mp) - LGA.laDRBperM) * ( 1.25 ) * mp * LGA.laPTSperPoss * LGA.laORBrate;
  const orbVal = ((orb / mp) - LGA.laORBperM)* ( 1.25 ) * mp * LGA.laPTSperPoss * LGA.laDRBrate;
  return volume + efficiency + astVal + stlVal + blkVal + tovVal + drbVal + orbVal;
}

export function computeMatchups(winners) {
  const t = {};
  BRACKET.r1.forEach((s) => (t[s.id] = s.teams.slice()));
  const resolve = (id) => winners[id];
  BRACKET.r2.forEach((s) => (t[s.id] = s.from.map(resolve)));
  BRACKET.r3.forEach((s) => (t[s.id] = s.from.map(resolve)));
  BRACKET.r4.forEach((s) => (t[s.id] = s.from.map(resolve)));
  return t;
}

export function potentialPoints(winTeam, loseTeam, roundKey) {
  const base = ROUND_BASE[roundKey];
  const diff = winTeam.seed - loseTeam.seed;
  const bonus = diff > 0 ? diff : 0;
  return { base, bonus, total: base + bonus };
}

// computePoints / playoff-bracket helpers omitted from this snapshot — they
// are not relevant to the nba-stats-web port. See upstream
// tmayhew1/playoff-tracker:app/scoring.js for the full implementation if
// needed.
