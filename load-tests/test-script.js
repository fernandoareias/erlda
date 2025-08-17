import http from 'k6/http';
import { check } from 'k6';

export let options = {
  vus: 1000, // Número de usuários virtuais
  duration: '60s',
};

export default function () {
  let res = http.get('http://localhost:8080/index.html');
  check(res, {
    'status was 200': (r) => r.status === 200,
  });
}