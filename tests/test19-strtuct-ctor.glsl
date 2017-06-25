struct Data
{
  float first;
  vec2 second;
};

Data dataValue = Data(1.4, vec2(16.0, 22.5));

Data dataArray[3] = Data[3](
  Data(1.0, vec2(-19.0, 4.5)),
  Data(-3.0, vec2(2.718, 2.0)),
  Data(29.5, vec2(3.142, 3.333)));

Data dataArray[3] = {
  {1.0, {-19.0, 4.5} },
  {-3.0, {2.718, 2.0} },
  {29.5, {3.142, 3.333} } };
