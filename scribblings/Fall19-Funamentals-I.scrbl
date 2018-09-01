#lang scribble/manual
@require[@for-label[
@;neu-fall18 
racket 2htdp/universe]]

@title{neu-fall18}
@author{Matthias Felleisen}

@defmodule[neu-fall18]

The teachpack provides auxiliary functions for Northeastern's Fall 2018
Fundamentals I course (regular edition). 

A @deftech{Response} is one of: 
@itemlist[

@item{@defthing[LIKE string?]{the listener liked the song}}
@item{@defthing[DONT string?]{the listener did not like the song}}
@item{@defthing[DONE string?]{the listener did not provide feeback}}
]

@defproc[(file-as-bytes [s string??]) bytes?]{
 retrives a file as a byte string.}

@defproc[(play-sound [s bytes??]) #, @tech{Response}]{
 plays the given MP3 bytes on your computer's speakers. 

@bold{Effect} It pops up a @racket[big-bang] window that allows listeners
to make the following choices via mouse clicks: 
@itemlist[
@item{pause the MP3 rendering}
@item{resume a paused MP3 rendering}
@item{"like" the MP3, and}
@item{"dislike" the MP3.}
]}

@defstruct[song-bytes ([title string?][mp3 bytes?])]{
 represents MP3 as they are sent from the Fundamentals I server. 
}

@history[
 #:changed "0.9" @list{Sat Sep  1 13:46:50 EDT 2018}
]

