export const runtime = "nodejs";
export const maxDuration = 15;

// NOTE: Same Cache-Control pattern as scores/route.js, longer windows for
// less-volatile data:
//   "Cache-Control": "public, s-maxage=20, stale-while-revalidate=60"
//
// Also includes a useful ISO-8601 duration parser (parseMinutes) — not
// directly needed by nba-stats-web (BBR gamelogs report minutes as a
// decimal already), but kept here for reference.

function parseMinutes(iso) {
  if (!iso) return 0;
  const m = /PT(?:(\d+)M)?(?:(\d+(?:\.\d+)?)S)?/.exec(iso);
  if (!m) return 0;
  const mins = parseInt(m[1] || "0", 10);
  const secs = parseFloat(m[2] || "0");
  return mins + secs / 60;
}

// Player-row mapping pattern — useful as a reference shape for
// lib/data/schemas.ts BoxscoreRow:
//   { name, starter, oncourt, mp, pts, reb, drb, orb, ast, stl, blk, tov,
//     fgm, fga, tpm, tpa, ftm, fta, plusMinus }
