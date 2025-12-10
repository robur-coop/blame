# blame, a search engine about emails as an unikernel

Blame is a web app that offers an email search service from an archive. This
project is related to [`blaze`][blaze] and its email archiving system. From an
archive, the user can launch the `blame.hvt` unikernel ([Solo5][solo5]) to
obtain a web interface (on the specified IPv4 address) that allows them to
search for emails by keyword.

For more details about this unikernel, you can read [our article][blame-article]
about how we developped it and what is the outcome.

To use it, you will need to install `blaze` and `solo5` in order to create an
archive and launch the unikernel.

`blaze` can create an archive from emails (in CRLF format) such as:
```shell
$ cat >emails.txt<<EOF
path/to/my/email0.eml
path/to/my/email1.eml
path/to/my/email2.eml
...
EOF
$ blaze pack make -o pack.pack --align=512 < emails.txt
```

For more details on the archive system, please refer to [our article][carton]
on this subject.

Next, you need to create a tap interface so that it can be used by the
unikernel and so that the latter is accessible from the network.
```shell
sudo ip link add name service type bridge
sudo ip addr add 10.0.0.1/24 dev service
sudo ip tuntap add name tap0 mode tap
sudo ip link set tap0 master service
sudo ip link set service up
sudo ip link set tap0 up
```

You need to install `blame` with `opam pin` (it's actually not possible to
install `blame` with a sandboxed environment). Finally, you can launch the
unikernel with Solo5 and your archive:
```shell
$ opam pin add https://github.com/robur-coop/blame
$ solo5-hvt --mem=512 --net:service=tap0 --block:archive=pack.pack -- \
  $(opam var bin)/blame.hvt --ipv4=10.0.0.2/24 --color=always
```

A website is then accessible at the specified address (in our example,
10.0.0.2). The webapp starts by downloading (from the unikernel) a whole bunch
of information and finally offers a search bar and a list of emails
(downloadable) available from your archive.

If you would like more details about the search engine, please read [our
article][stem] on this topic.

[stem]: https://blog.robur.coop/articles/ptt_stem_and_search_engine.html
[carton]: https://blog.robur.coop/articles/2025-01-07-carton-and-cachet.html
[blaze]: https://github.com/dinosaure/blaze
[solo5]: https://github.com/Solo5/solo5/
[blame-article]: https://blog.robur.coop/articles/2025-04-12-ptt-search-webapp.html
