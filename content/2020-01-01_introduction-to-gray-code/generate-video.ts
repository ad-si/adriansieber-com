import { renderVideo } from "declavid"

// Extract flags passed on the CLI
const flags = process.argv.slice(2)

const isSmall = flags.includes("--small")

renderVideo({
  width: isSmall ? 480 : 1920,
  height: isSmall ? 270 : 1080,
  fileName: `./video${isSmall ? "-small" : ""}.mp4`,
  clips: [
    {
      type: "narration",
      text: `
        Have you ever seen this crazy binary pattern?
      `,
    },
    {
      type: "narration",
      text: `
        It was invented in 1947 by Frank Gray
        and is therefore called "Gray Code".
      `,
    },
  ],
})
