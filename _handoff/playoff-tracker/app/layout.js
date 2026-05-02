import "./globals.css";

export const metadata = {
  title: "2026 Playoff Draft Tracker",
  description: "Spencer vs. Trey · live projected points",
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
