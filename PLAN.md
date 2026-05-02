# Build `nba-stats-web`: Next.js port of the nba-projects Shiny app

## Context

You maintain two NBA apps:

1. **`tmayhew1/playoff-tracker`** — a Next.js 14 / React 18 / Tailwind app that
   scrapes `cdn.nba.com` from API routes, renders a live playoff bracket, and
   already contains a partial port of the Shiny "Value Added" math (in
   `app/scoring.js` and the `BoxscoreTable` expandable rows).
2. **`tmayhew1/nba-projects`** — a single-file (~120 KB) R Shiny app with 9+
   tabs, backed by Basketball-Reference scraping (`rvest`), local CSV caches,
   and ggplot2/plotly/DT visuals.

The goal is **a third, brand-new repo (`nba-stats-web`)** that uses
playoff-tracker's framework conventions (Next.js 14 + React 18 + Tailwind +
typed API routes) to replicate the Shiny app's information and visuals as a
modern web app. The Shiny app keeps running unchanged — no migration deadline.

Key decisions already made:
- **All 9 tabs** ported.
- **Brand-new repo** (not a subfolder of either existing one).
- **TypeScript everywhere** (full TS, not the JS playoff-tracker uses).
- **Phase 1 reads existing CSVs**, Phase 2 ports `totals_collect.R` to TS.
- **Visx / D3** for charts, **TanStack Table** for grids.
- **Keep the Shiny app alive.**

## Constraint to address before any push

GitHub MCP scope is currently `tmayhew1/nba-projects` and
`tmayhew1/playoff-tracker` only. You must create the new repo
(`tmayhew1/nba-stats-web` or your preferred name) and add it to the allowed
scope before I can push. Until then, scaffold can be staged on the
`claude/combine-playoff-apps-2Rgc3` branch of `nba-projects` for review (that
branch doesn't exist yet — only `main` does).

## Repo scaffold

```
nba-stats-web/
  app/
    layout.tsx, page.tsx (Tab 1 = leaders), globals.css
    leaders/page.tsx
    compare-seasons/page.tsx        # Tab 2
    compare-runs/page.tsx           # Tab 3
    game/[gameId]/page.tsx          # Tab 4
    game-lookup/page.tsx            # Tab 5
    yesterday/page.tsx              # Tab 6
    careers/page.tsx                # Tab 7
    careers/playoffs/page.tsx       # Tab 7.5
    value/page.tsx                  # Tab 8
    players/[slug]/page.tsx         # Tab 9
    api/
      totals/route.ts        shooting/route.ts
      player/[slug]/route.ts gamelog/route.ts
      teamgamelog/route.ts   boxscore/route.ts
      yesterday/route.ts     meta/route.ts
  components/
    Nav.tsx
    charts/  ChartFrame, LineChart, ScatterPlot, BarChart,
             StackedBar, ShotChart, HeatmapZones, FourFactorRadar
    tables/  StatTable, ColumnRenderers
    filters/ SeasonPicker, PlayerPicker, StatPicker, ToggleGroup
    profile/ PlayerHeader, FourFactorPanel, ValueBreakdownPanel
  lib/
    data/    totals.ts, shooting.ts, gamelogs.ts, boxscore.ts,
             cache.ts, schemas.ts
    metrics/ valueAdded.ts, fourFactors.ts, ratings.ts, leagueAverages.ts
    scrape/  bbr.ts (cheerio), http.ts (rate-limit, UA, retry)
    theme/   teamColors.ts, contrast.ts, lighten.ts
    util/    seasons.ts, slug.ts, csv.ts, urlState.ts
  data/seed/
    Totals_s_latest.csv, Totals_p_latest.csv,
    avgsSummary.csv, team_hex_colors.csv,
    team_abbreviations.csv, menu_options*.csv, scoring_levels.csv
  next.config.js, tailwind.config.ts, postcss.config.js,
  tsconfig.json, package.json, .env.local.example, README.md
```

Dependencies on top of the playoff-tracker baseline: `typescript`,
`@types/{react,node}`, `papaparse` (+ types), `cheerio`,
`@tanstack/react-table`, `@visx/{xychart,scale,axis,group,shape,heatmap,
tooltip,responsive,curve}`, `chroma-js` (+ types), `zod`, `clsx`.

## Tab → route → API mapping

| Shiny tab | Route | API route | Per-tab spec to read |
|---|---|---|---|
| 1 Season Leaders | `/leaders` | `/api/totals?season&playoffs` | `tab 1 - NBA Season Leaders.R` |
| 2 Compare Seasons | `/compare-seasons` | `/api/totals` (multi-fetch) | `tab 2 - ...R` |
| 3 Compare Runs | `/compare-runs` | `/api/totals?seasonRange=` | `tab 3 - ...R` |
| 4 Single Game Perf | `/game/[gameId]` | `/api/boxscore?id=` | `tab 4 - ...R` |
| 5 Single Game Lookup | `/game-lookup` | `/api/teamgamelog`, `/api/gamelog` | `tab 5 - ...R` |
| 6 Yesterday | `/yesterday` | `/api/yesterday` | `tab 6 - ...R` |
| 7 Career Comp | `/careers` | `/api/player/[slug]?type=regular` | `tab 7 - ...R` |
| 7.5 Career Comp Playoffs | `/careers/playoffs` | `/api/player/[slug]?type=playoff` | `tab 7.5 - ...R` |
| 8 Value Breakdown | `/value` | `/api/totals` + `valueAdded.ts` | `tab 8 - ...R` |
| 9 Player Profile + 4F | `/players/[slug]` | `/api/player/[slug]`, `/api/totals` | `tab 9 - ...R` |

Default to **server components** for page shells (read CSV at request time,
render server-side); use **client components** only for interactive widgets,
all `@visx` charts, and all `@tanstack/react-table` instances. Same split
playoff-tracker uses (`page.js` server-side, polling client logic inline).

## Phase 1 — CSV-backed data layer

- Commit the newest dated dump from `nba-projects/Complete Data/` as
  `data/seed/Totals_s_latest.csv` and `Totals_p_latest.csv`. Also commit
  `team_hex_colors.csv`, `team_abbreviations.csv`, `avgsSummary.csv`,
  `scoring_levels.csv`, and the `menu_options*.csv` lookups.
- `lib/data/schemas.ts` — `zod` schemas for `TotalsRow`, `ShootingRow`,
  `BoxscoreRow`, `GamelogRow`, `TeamHex`, `MenuOption`. Type aliases via
  `z.infer`.
- `lib/data/totals.ts` — `loadTotals({ season, playoffs }): Promise<TotalsRow[]>`
  using `fs/promises.readFile` + `Papa.parse(csv, { header: true,
  dynamicTyping: true, skipEmptyLines: true })`, validated with the zod
  schema. Memoized in-process by file `mtime` via `lib/data/cache.ts`.
- `app/api/totals/route.ts` mirrors playoff-tracker's
  `app/api/scores/route.js`: `export const runtime = "nodejs"; export const
  revalidate = 3600;` and `Cache-Control: public, s-maxage=3600,
  stale-while-revalidate=86400`. Validate query params with zod.
- `app/api/meta/route.ts` returns the merged menu/teams/hex catalog so client
  filters hydrate in one request.

## Phase 2 — TypeScript scraper (port of `totals_collect.R`)

`lib/scrape/bbr.ts`:
- `fetchSeasonTotals(year)` → `leagues/NBA_{year}_totals.html` (table
  `#totals_stats`).
- `fetchSeasonShooting(year)` → `NBA_{year}_shooting.html` (BBR wraps the
  table in HTML comments — strip `<!--` / `-->` before passing to cheerio,
  same trick rvest needs).
- `fetchPlayoffTotals(year)` → `playoffs/NBA_{year}_totals.html`.
- `fetchPlayerGamelog(slug, year, season)` → `players/{first}/{slug}/gamelog/
  {year}` with `season=regular|playoffs`.
- `fetchTeamGamelog(team, year)` → `teams/{TEAM}/{year}_games.html`.
- `fetchBoxscore(boxscoreId)` → `boxscores/{id}.html`.

`lib/scrape/http.ts` — shared fetch wrapper: 1 req / 3 s, real UA,
on-disk response cache under `.cache/bbr/{sha}.html` so local re-runs are
free.

Daily refresh via **GitHub Actions** (not Vercel cron — Hobby tier is 60 s
capped and the multi-season scrape blows past that).
`.github/workflows/refresh-totals.yml` runs nightly, executes
`scripts/refresh.ts`, writes `data/refresh/Totals_{s,p}_<date>.csv`, updates
`data/seed/INDEX.json`, and opens a PR via
`peter-evans/create-pull-request`. Route handlers only ever import
`loadTotals`, so swapping the writer to KV/Postgres later is contained.

## Value Added port (`lib/metrics/valueAdded.ts`)

Reference: `_handoff/playoff-tracker/app/scoring.js` — `valueAdd(p)` already
implements the eight components with the `LGA` constants table. Port
verbatim and add `valueAddedBreakdown(row)` returning
`{ volume, threes, twos, ft, ast, stl, blk, tov, drb, orb, total }` —
exactly the shape Tab 8's stacked bar and the `BoxscoreTable` expandable
rows in `_handoff/playoff-tracker/app/page.js` (see `VABreakdown` component)
consume. Per-season `LGA` constants live in
`lib/metrics/leagueAverages.ts`, seeded from `avgsSummary.csv`, exposed as
`getLeagueAverages(season): LGA`.

`lib/metrics/fourFactors.ts` — eFG%, TOV%, OREB%, FT/FGA + radar values for
Tab 9.

## Visx primitives (`components/charts/`)

Each primitive wraps `@visx/responsive ParentSize` and accepts
`{ data, xKey, yKey, colorBy?, tooltipFormat?, height? }`.

| Shiny chart | Tabs | Primitive | Visx packages |
|---|---|---|---|
| ggplot scatter w/ team colors | 1, 2, 8, 9 | `ScatterPlot` | xychart, scale, axis |
| Multi-season trend lines | 3, 7, 7.5 | `LineChart` | xychart, curve |
| Career bar comparisons | 7, 7.5 | `BarChart` | shape, scale, axis |
| Value Breakdown stacked bar | 8 | `StackedBar` (BarStack ×10 series) | shape/BarStack |
| Shot zones | 4, 5, 9 | `ShotChart` (court SVG + heatmap fills) | heatmap, group |
| 4-Factor radar | 9 | `FourFactorRadar` | shape/LinePath, scale |
| `patchwork` two-up panels | several | `<ChartFrame layout="grid">` | (CSS grid) |

Tooltips via `@visx/tooltip` `useTooltip` + `TooltipWithBounds`. All charts
are client components.

## Tables (replace DT)

`components/tables/StatTable.tsx` wraps TanStack Table v8 with `getCoreRowModel
+ getSortedRowModel + getFilteredRowModel`. Column meta carries
`{ format: 'pct'|'num'|'int'|'pm', tint?: 'team' }`. A `tint: 'team'` cell
reads the row's `Tm` value, looks up the hex in `lib/theme/teamColors.ts`,
calls `lighten(hex, 0.65)` (chroma.js port of R's `lighten_color()`), and
applies it as `backgroundColor` with `contrastText(bg)` deciding between
`#fff` and `#111`. This replicates `DT::formatStyle(... styleEqual(...,
lighten_color(team_hex_colors$Hex, 0.65)))` exactly.

## Theming

- `lib/theme/teamColors.ts` — `Record<string, string>` parsed from
  `data/seed/team_hex_colors.csv` (72 rows incl. legacy `MNL`, `STB`,
  multi-team `2TM`/`3TM`/etc. → `#000000`).
- `lib/theme/lighten.ts` — `chroma(hex).brighten(amount * 3).hex()` for
  parity with R's HCL-based lightening.
- `lib/theme/contrast.ts` — WCAG luminance picker.
- Tailwind: leave `theme.extend` empty (matches playoff-tracker); apply team
  colors via inline style — 70+ team classes would be wasteful.

## URL-driven state

`useUrlState<T>(key, parse, serialize)` hook in `lib/util/urlState.ts`,
backed by `useSearchParams` + `router.replace`. Used on Tabs 2, 3, 7, 7.5,
8, 9 so links are shareable, e.g.:

- `/compare-seasons?p1=lebronja01&s1=2013&p2=jordami01&s2=1991`
- `/value?players=...&season=&peak=true`
- `/players/[slug]?compare=jordami01&season=2025`

Tabs 1, 4, 5, 6 use path or simple search params.

## Caching

- `/api/totals`, `/api/shooting`, `/api/meta`, `/api/player`, `/api/gamelog`,
  `/api/teamgamelog` → `revalidate=3600`, `s-maxage=3600`,
  `stale-while-revalidate=86400` (daily refresh cadence).
- `/api/yesterday` → `s-maxage=300, stale-while-revalidate=3600`.
- `/api/boxscore` → `s-maxage=86400` (immutable once posted).
- Pages: leaders/value/careers `export const revalidate = 3600`; player
  profile uses `generateStaticParams` over the active season's roster (~500
  pages) with ISR fallback.

These mirror playoff-tracker's `s-maxage=30, stale-while-revalidate=60` for
live data and longer windows for static.

## Deployment

- New Vercel project on the new repo, `main` → prod.
- Env vars: `BBR_USER_AGENT` (required), `BBR_RATE_LIMIT_MS` (default 3000),
  `NEXT_PUBLIC_DEFAULT_SEASON`.
- `data/seed/` is source-of-truth at build time; `data/refresh/` is
  scraper output (gitignore historical dumps if bundle bloats past 100 MB).
- README must call out: BBR rate-limits aggressively. Scraper runs only in
  the GitHub Action; runtime route handlers only read CSVs.

## Phased rollout

- **A. Scaffold + Tab 1 end-to-end** — init TS Next.js, port
  `_handoff/playoff-tracker/app/teams.js` → `lib/theme/teamColors.ts`, port `_handoff/playoff-tracker/app/scoring.js
  valueAdd` → `lib/metrics/valueAdded.ts`, build `loadTotals` +
  `/api/totals`, build `StatTable` with team-tint, ship `/leaders`. Visual
  diff vs `tab 1 - NBA Season Leaders.R`.
- **B. Player comparisons + careers** — Tabs 2, 3, 7, 7.5. Add `LineChart`,
  `BarChart`, `ScatterPlot`. URL state hook lands here.
- **C. Game-level lookups** — Tabs 4, 5, 6.
- **D. Value Breakdown + Player Profile** — Tabs 8, 9. `StackedBar`,
  `FourFactorRadar`, `ValueBreakdownPanel`.
- **E. TS scraper replaces seed CSVs** — `lib/scrape/bbr.ts`, GitHub Action,
  `loadTotals` reads `data/refresh/<latest>.csv` first, falls back to seed.

## Verification per phase

- Pin one reference player per tab (Jokic 2025-26 for Tab 1, LeBron vs
  Jordan for Tab 7) and visually diff the React output side-by-side with
  the Shiny tab.
- Every PR runs `npm run dev` smoke + `npm run typecheck` (`tsc --noEmit`)
  + `npm run lint` (`next lint`).
- Phase A: hand-pick a roster, assert `valueAdded(row)` matches
  `_handoff/playoff-tracker/app/scoring.js valueAdd(p)` to 6 decimals.
- Phase E: `diff <(sort old.csv) <(sort new.csv)` between Shiny-produced
  and TS-produced totals to catch column-name or rounding drift.

## Reference files to read while implementing

**playoff-tracker** (framework template — snapshot at `_handoff/playoff-tracker/`):
- `app/page.js` — boxscore + Value Added expandable breakdown UI pattern (`VABreakdown` component)
- `app/scoring.js` — exact `LGA` constants and `valueAdd()` math
- `app/teams.js` — constants module convention
- `app/api/scores/route.js`, `app/api/boxscore/route.js` — cache-control conventions
- `tailwind.config.js`, `next.config.js`, `package.json` — versions and minimalism

**nba-projects** (content source, this repo, `main` branch):
- `app.R` — UI shell, DT formatting, `lighten_color()`
- `totals_collect.R` — full scrape pipeline (spec for `lib/scrape/bbr.ts`)
- `eda.R` — derived columns helpers
- `tab 1 ... tab 9 ...R` — per-tab acceptance specs (column lists, plot specs, filter widgets)
- `Complete Data/team_hex_colors.csv` — verbatim source for `teamColors.ts`
- `Complete Data/avgsSummary.csv` — per-season league averages
- newest-dated `Complete Data/Totals_s_*.csv` and `Totals_p_*.csv` — copy as `data/seed/Totals_{s,p}_latest.csv`
- `Complete Data/menu_options*.csv`, `team_abbreviations.csv`, `scoring_levels.csv` — lookups for `/api/meta`

## Critical files to create

- `lib/data/totals.ts`
- `lib/metrics/valueAdded.ts`
- `lib/theme/teamColors.ts`
- `app/api/totals/route.ts`
- `lib/scrape/bbr.ts`
- `components/tables/StatTable.tsx`
- `components/charts/StackedBar.tsx`
