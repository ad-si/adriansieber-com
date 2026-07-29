import { renderVideo } from "declavid"

// Extract flags passed on the CLI
const flags = process.argv.slice(2)

const isSmall = flags.includes("--small")

renderVideo({
  width: 1920,
  height: 1080,
  filePath: `./video${isSmall ? "-small" : ""}.mp4`,
  isDraft: isSmall,
  clips: [
    {
      type: "image",
      filePath: "images/gray-code-disc.png",
      narration: {
        text: `
          Have you ever seen this crazy binary pattern
          consisting of 1s and 0s
          arranged in a very particular way?
        `,
      },
    },
    {
      type: "image",
      filePath: "images/frank-gray.jpeg",
      narration: {
        text: `
          It was invented in 1947 by Frank Gray
          and is therefore called "Gray Code".
        `,
      },
    },
    {
      type: "image",
      filePath: "images/rotary-encoder.png",
      narration: {
        text: `
          It's used in many applications,
          for example in rotary encoders like this one.
        `,
      },
      padEnd: 2,
    },
    {
      type: "video",
      filePath: "disc-animation-babylonjs/video.mp4",
      narration: {
        text: `
          Here you can see a simple rotary encoder in action.
          By blocking the light of the LEDs,
          sensors on the other side of the disc can detect the patterns
          and determine the rotation angle
        `,
      },
    },
    {
      type: "text",
      isNarrated: true,
      text: `
        And it's used in the digital world
        for error detection.
      `,
    },
    {
      type: "text",
      isNarrated: true,
      text: `But how does it work?`,
    },
    {
      type: "image",
      filePath: "images/binary-code-changes.png",
      narration: {
        text: `
          Let's start with a simple binary number.
          If we count up,
          more than one bit can change at a time.
        `,
      },
    },
    {
      type: "image",
      filePath: "images/gray-code-changes.png",
      narration: {
        text: `
          In Gray Code, however,
          only one bit changes at a time.
        `,
      },
    },
  ],
})
