FROM alpine:3.18

ADD bs-gen bs-gen

CMD [ "./bs-gen" ]
