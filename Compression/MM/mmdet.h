// MM type detection library

// WAV header recognition
int autodetect_wav_header (void *buf, long size, int *is_float, int *num_chan, int *word_size, int *offset);
// Call to MM detector. channels[] - variants of number of channels it should try
int autodetect_by_entropy (void *buf, int bufsize, int channels[], int bitvalues[], double min_entropy, int *is_float, int *num_chan, int *word_size, int *offset);

