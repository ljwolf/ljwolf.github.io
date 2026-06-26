module.exports = {
  content: [
    "layouts/**/*.html",
    "../../content/**/*.md",
  ],
  theme: {
    container: {
      center: true,
      screens: {
        sm: '40rem',
        md: '40rem',
        lg: '40rem',
        xl: '45rem',
        '2xl': '50rem',
      },
    },
    extend: {
      letterSpacing: {
        wide: ".03em"
      },
      lineHeight: {
        'loose': '1.7',
      },
      flexGrow: {
        '2': 2
      },
            flexShrink: {
        '2': 2
      },
      colors: {
        'tp-black': 'rgba(0,0,0,.8)',
        // Light end (50-200, used for light-mode backgrounds/borders) is plain
        // neutral grey. Dark end (300-900, dark-mode backgrounds/text) keeps a
        // muted, desaturated charcoal. Aliasing both names means every existing
        // zinc-*/neutral-* class across the templates stays in sync.
        zinc: {
          50: '#fafaf9',
          100: '#f5f5f4',
          200: '#e7e5e4',
          300: '#b3ad95',
          400: '#8f8a76',
          500: '#6f6b5c',
          600: '#565347',
          700: '#3d3b33',
          800: '#24272a',
          900: '#16181a',
        },
        neutral: {
          50: '#fafaf9',
          100: '#f5f5f4',
          200: '#e7e5e4',
          300: '#b3ad95',
          400: '#8f8a76',
          500: '#6f6b5c',
          600: '#565347',
          700: '#3d3b33',
          800: '#24272a',
          900: '#16181a',
        },
        // Disco-Elysium-leaning orange. Saturated/true-orange hue so it reads
        // vibrant in light mode (not brown), while 700 stays dark enough to keep
        // link text legible on the light-grey bg. 300 unchanged (dark-mode links).
        accent: {
          300: '#d8a873',
          500: '#d9741a',
          600: '#c25d12',
          700: '#a8500a',
          800: '#823c08',
        },
      },
      fontFamily: {
        serif: ["IBM Plex Serif", "Georgia", "Times New Roman", "Times", "serif"],
        mono: ["IBM Plex Mono", "ui-monospace", "SFMono-Regular", "Menlo", "Monaco",
               "Consolas", "Liberation Mono", "Courier New", "monospace"],
      },
      gridTemplateColumns: {
        'nav': 'repeat(auto-fill, minmax(80px, 1fr))',
        'minimal-list-item': '15% 85%',
        'list-item': '.6fr 3fr auto 1fr',
      }
    }
  },
  plugins: [],
  darkMode: 'class',
}
