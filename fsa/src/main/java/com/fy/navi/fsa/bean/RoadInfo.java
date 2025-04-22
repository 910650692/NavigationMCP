package com.fy.navi.fsa.bean;

/**
 *1.1.35、获取NOP导航下个路段和下下路段
 *
 *
 * RoadInfo的json字符串
 * [{"tbt":0,"name":"京沪高速","meter":0,"trafficStatus":0,"interChange":"","anpType":true},{"tbt":0,"name":"荣乌高速","meter":0,"trafficStatus":0,"interChange":"","anpType":false}]
 * Json数组第一个是下个路段；第二个是下下个路段。
 * 如果没有下下个路段，Json数组只返回下个路段数据。
 * json解析使用name和anpType；其他的字段不用不解析使用。
 */
public class RoadInfo {
    //名字
    private String name;
    private boolean anpType;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean isAnpType() {
        return anpType;
    }

    public void setAnpType(boolean anpType) {
        this.anpType = anpType;
    }
}
