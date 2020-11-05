FROM openjdk:13-alpine
COPY ./* /app/
WORKDIR /app/
RUN javac -d ./output ./Test3.java
WORKDIR /app/output
