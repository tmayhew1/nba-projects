export const runtime = "nodejs";
export const maxDuration = 15;
export const revalidate = 30;

// NOTE: This is the canonical pattern for runtime + revalidate + Cache-Control
// headers in nba-stats-web API routes. The fetch wrapper, headers map, and
// the Response construction at the bottom (s-maxage=30, stale-while-revalidate=60)
// are what to mirror in lib/scrape/http.ts and the route handlers.
//
// nba-stats-web won't fetch NBA.com — it reads CSVs (Phase 1) or scrapes BBR
// (Phase 2) — so the schedule/scoreboard merging logic isn't relevant.
// What matters: the cache headers and the runtime declaration.

const HEADERS = {
  "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  Accept: "application/json, text/plain, */*",
  "Accept-Language": "en-US,en;q=0.9",
  Referer: "https://www.nba.com/",
  Origin: "https://www.nba.com",
  "x-nba-stats-origin": "stats",
  "x-nba-stats-token": "true",
};

async function fetchJson(url, timeoutMs = 5000) {
  const ctrl = new AbortController();
  const t = setTimeout(() => ctrl.abort(), timeoutMs);
  try {
    const res = await fetch(url, { headers: HEADERS, signal: ctrl.signal, cache: "no-store" });
    if (!res.ok) throw new Error(`${res.status}`);
    return await res.json();
  } finally {
    clearTimeout(t);
  }
}

// Response shape for reference — implementation body omitted:
//   return new Response(JSON.stringify(body), {
//     status: 200,
//     headers: {
//       "Content-Type": "application/json",
//       "Cache-Control": "public, s-maxage=30, stale-while-revalidate=60",
//     },
//   });
