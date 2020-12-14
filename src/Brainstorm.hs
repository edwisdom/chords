-- Placeholder code
newtype Note = Note {getN :: Int}
newtype Interval = Interval {getI :: Int}
newtype Chord = Chord {getC :: Int}


-- | We might wanna make a type class of things convertible
-- to a frequency value. For this simple type, it's just the one field.
--
-- All of these datatypes are also places where we can return distances
-- given 2 values. For pitch, we can return 3 types of distances; for MIDI, 2;
-- for frequencies, 1.
--
-- Frequency as a number of hertz.
newtype Frequency = Frequency {hertz :: Int}

-- | Smart constructor for frequency, checks for i > 0.
frequencyFrom :: Int -> Frequency
frequencyFrom i = undefined

-- | A MIDI note has a number.
newtype MIDI = MIDI {getNum :: Int}

-- | Smart constructor for MIDI notes, checks for 0 <= i < 128.
midiFrom :: Int -> Maybe MIDI
midiFrom = undefined

-- | Convert a MIDI to a frequency.
midiToFreq :: MIDI -> Frequency
midiToFreq = undefined

-- | Transpose a MIDI note by some amount. We could also use intervals here if needed.
transposeM :: MIDI -> Int -> MIDI
transposeM = undefined

-- | Pitch in scientific pitch notation (SPN)
data Pitch = Pitch {getNote :: Note, octave :: Int}

-- | Smart constructor for Pitch
pitchFrom :: Note -> Int -> Pitch
pitchFrom = undefined

-- | Convert pitch to a MIDI
pitchToMIDI :: Pitch -> MIDI
pitchToMIDI = undefined

-- | Convert pitch to a frequency
pitchToFreq :: Pitch -> Frequency
pitchToFreq = midiToFreq . pitchToMIDI

-- | Transpose a pitch by some interval. We could also use integers here if needed.
transposeP :: Pitch -> Interval -> Pitch
transposeP = undefined

-- | Convert MIDI to the simplest possible pitches (i.e. using the fewest accidentals)
midiToPitches :: MIDI -> [Pitch]
midiToPitches = undefined

-- | A chord with a bass note (not the best name, but hey) that can be specified or not
data ChordWithBass = ChordWithBass {getChord :: Chord,
                                    getBass :: Maybe Note}

-- | Checks whether the voicing is in root position (i.e. bass note is chord root)
isRootPosition :: ChordWithBass -> Bool
isRootPosition = undefined

-- | A voicing is a chord with a bass note potentially specified, and a list of pitches.
data Voicing = Voicing {getCWB :: ChordWithBass,
                        getPitches :: [Pitch]}

-- | Smart constructor for voicings.
voicingFrom :: ChordWithBass -> [Pitch] -> Maybe Voicing
voicingFrom = undefined

-- | There will be a number of such functions for different voicing strategies.
generateVoicing :: ChordWithBass -> [Pitch]
generateVoicing = undefined

-- | Given two voicings, returns the voice leading distance between them in half steps.
voiceLeadingDistance :: Voicing -> Voicing -> Int
voiceLeadingDistance = undefined

-- | Given a voicing and a chord we want to reach, find the voicing with the minimum voice leading distance.
voiceLeadFromTo :: Voicing -> ChordWithBass -> Voicing
voiceLeadFromTo = undefined

newtype Progression = Progression {getChords :: [ChordWithBass]}

-- | Given a chord progression and an initial voicing, voice lead each successive chord
-- to minimize voice leading distance from the previous one.
progressionToVoicings :: Progression -> Voicing -> [Voicing]
progressionToVoicings = undefined

-- | Tritone sub an entire progression by subbing each individual chord.
-- We can come up with a number of different substitutions that operate on
-- the whole progression rather than just individual chords.
tritoneSub :: Progression -> Progression
tritoneSub = undefined

-- | Given a progression, a number of beats per bar, and a list of
-- durations (in number of beats), i.e. one duration per chord in progression,
-- we can create a lead sheet datatype.
data LeadSheet = LeadSheet {getProg :: Progression,
                            getMeter :: Int,
                            getDurations :: [Int]}

