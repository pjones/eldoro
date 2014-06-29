# Eldoro: A pomodoro timer/tracker that works with org-mode.

A major mode for showing pomodoro timers.

Eldoro works with tasks defined in an `org-mode` buffer.  When
you start Eldoro from within an `org-mode` buffer it will gather
all of headings which are children of the heading that point is
currently on.  You can then start and stop pomodoro and break
timers from within the Eldoro buffer.

## Getting Started

First, create an `org-mode` outline that looks something like
this:

~~~
* Tasks I Want To Do
** Write angry letter to Congress
** Find a bug in OpenSSL
** Upload compromising photos to FB
~~~

Now, move point to the first-level heading and start Eldoro with
the `eldoro` interactive function.  Move point to a task you want
to work on and press RET.

To switch to a break, just press RET again.  If someone
interrupts you, press i.

![Screenshot](http://www.pmade.com/static/images/2014/a5fa5925980289be7d83d3e8dbe31e1c.png)

## Reporting

By default, Eldoro writes some basic statistics into `org-mode`
properties.  If you want to compare the number of pomodori from
day to day make sure you create new headings in the `org-mode`
buffer every day.  Eldoro does not currently record timestamps
with its statistics.  It would be nice if there was a better Org
API for logging Eldoro statistics into a drawer.

This mode runs the hook `eldoro-mode-hook`, as the final step
during initialization.

key             binding
---             -------

TAB		eldoro-jump-to-heading
RET		eldoro-next-action
b		bury-buffer
g		eldoro-update
h		eldoro-toggle-help
i		eldoro-interruption
q		eldoro-quit
r		eldoro-reset-statistics
s		eldoro-stop-clock


