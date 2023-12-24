# https://en.wikipedia.org/wiki/General_MIDI#Program_change_events
instruments <- c(
  # Piano
  "Acoustic Grand Piano",
  "Bright Acoustic Piano",
  "Electric Grand Piano",
  "Honky-Tonk Piano",
  "Electric Piano 1",
  "Electric Piano 2",
  "Harpsichord",
  "Clavinet",

  # Chromatic percussion
  "Celesta",
  "Glockenspiel",
  "Music Box",
  "Vibraphone",
  "Marimba",
  "Xylophone",
  "Tubular Bells",
  "Dulcimer",

  # Organ
  "Drawbar Organ",
  "Percussive Organ",
  "Rock Organ",
  "Church Organ",
  "Reed Organ",
  "Accordion",
  "Harmonica",
  "Tango Accordion",

  # Guitar
  "Acoustic Guitar (Nylon)",
  "Acoustic Guitar (Steel)",
  "Electric Guitar (Jazz)",
  "Electric Guitar (Clean)",
  "Electric Guitar (Muted)",
  "Overdriven Guitar",
  "Distortion Guitar",
  "Guitar Harmonics",

  # Bass
  "Acoustic Bass",
  "Electric Bass (Finger)",
  "Electric Bass (Pick)",
  "Fretless Bass",
  "Slap Bass 1",
  "Slap Bass 2",
  "Synth Bass 1",
  "Synth Bass 2",

  # Strings
  "Violin",
  "Viola",
  "Cello",
  "Contrabass",
  "Tremolo Strings",
  "Pizzicato Strings",
  "Orchestral Harp",
  "Timpani",

  # Ensemble
  "String Ensemble 1",
  "String Ensemble 2",
  "Synth Strings 1",
  "Synth Strings 2",
  "Choir Aahs",
  "Voice Oohs",
  "Synth Voice",
  "Orchestra Hit",

  # Brass
  "Trumpet",
  "Trombone",
  "Tuba",
  "Muted Trumpet",
  "French Horn",
  "Brass Section",
  "Synth Brass 1",
  "Synth Brass 2",

  # Reed
  "Soprano Sax",
  "Alto Sax",
  "Tenor Sax",
  "Baritone Sax",
  "Oboe",
  "English Horn",
  "Bassoon",
  "Clarinet",

  # Pipe
  "Piccolo",
  "Flute",
  "Recorder",
  "Pan Flute",
  "Blown Bottle",
  "Shakuhachi",
  "Whistle",
  "Ocarina",

  # Synth lead
  "Lead 1 (Square)",
  "Lead 2 (Sawtooth)",
  "Lead 3 (Calliope)",
  "Lead 4 (Chiff)",
  "Lead 5 (Charang)",
  "Lead 6 (Voice)",
  "Lead 7 (Fifths)",
  "Lead 8 (Bass + Lead)",

  # Synth pad
  "Pad 1 (New Age)",
  "Pad 2 (Warm)",
  "Pad 3 (Polysynth)",
  "Pad 4 (Choir)",
  "Pad 5 (Bowed)",
  "Pad 6 (Metallic)",
  "Pad 7 (Halo)",
  "Pad 8 (Sweep)",

  # Synth effects
  "FX 1 (Rain)",
  "FX 2 (Soundtrack)",
  "FX 3 (Crystal)",
  "FX 4 (Atmosphere)",
  "FX 5 (Brightness)",
  "FX 6 (Goblins)",
  "FX 7 (Echoes)",
  "FX 8 (Sci-Fi)",

  # Ethnic
  "Sitar",
  "Banjo",
  "Shamisen",
  "Koto",
  "Kalimba",
  "Bag Pipe",
  "Fiddle",
  "Shanai",

  # Percussive
  "Tinkle Bell",
  "Agogo",
  "Steel Drums",
  "Woodblock",
  "Taiko Drum",
  "Melodic Tom",
  "Synth Drum",
  "Reverse Cymbal",

  # Sound effects
  "Guitar Fret Noise",
  "Breath Noise",
  "Seashore",
  "Bird Tweet",
  "Telephone Ring",
  "Helicopter",
  "Applause",
  "Gunshot"
)
