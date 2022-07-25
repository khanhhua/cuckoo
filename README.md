# cuckoo
What is so special about cuckoo? It is the eggs.

## Synopsis

This library generates combinations of randomized data for any giving templates which should mimic the your project data model and mass generate them for testing purpose.

You could either use it as a docker image

```sh
docker pull khanhhua/cuckoo:latest
docker run -p 3000:3000 cuckoo:latest
```

or use it as a library :D

**Request**

```sh
curl --location --request POST 'http://localhost:3000/' \
--header 'Content-Type: application/json' \
--data-raw '{
    "customer": "fullname",
    "primary_email": "email",
    "secondary_email": "email",
    "home_address": "address",
    "dob": "past-date",
    "employer": {
        "name": "company",
        "domain": "domain"
    }
}'
```

**Response**
```json
[
    {
        "customer": "Mia Robinson",
        "dob": "2011-09-25",
        "employer": {
            "domain": "youronlinechoices.com",
            "name": "Pfizer"
        },
        "home_address": "946 Albemarle Avenue, Oklahoma",
        "primary_email": "mia.tierradelfuego_2266@bing.com",
        "secondary_email": "james.kyushu_9063@researchgate.net"
    },
    {
        "customer": "Benjamin Campbell",
        "dob": "1984-11-23",
        "employer": {
            "domain": "rambler.ru",
            "name": "Verizon Communications"
        },
        "home_address": "453 Aalto Place, Maine",
        "primary_email": "liam.redrock_8292@www.over-blog.com",
        "secondary_email": "william.melville_9613@nps.gov"
    }
]
```