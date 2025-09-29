export def ffmpeg-davinci [file] {
  let base_out = ($file | path parse | select stem)
  let out = $"($base_out).mov"
  ffmpeg -i $file -c:v dnxhd -profile:v dnxhr_hq -c:a pcm_s16le -pix_fmt yuv422p $out
}

export def ff [...args] {
  firefox --new-tab $"https://duckduckgo.com/?q=($args | str join ' ')"
};

export def ffw [...args] {
  firefox --new-window $"https://duckduckgo.com/?q=($args | str join ' ')"
};
