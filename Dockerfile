FROM alpine:3.18

ADD bs_gen bs_gen

CMD [ "./bs_gen" ]
