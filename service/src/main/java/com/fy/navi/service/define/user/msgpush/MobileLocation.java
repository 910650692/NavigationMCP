package com.fy.navi.service.define.user.msgpush;

public class MobileLocation {
    public int type; //起点处道路类型 @range [ 0: 普通路, 1: 高架上, 2: 高架下, 3: 主路, 4: 辅路, 5: 隧道, 7: 环岛 ]
    public int mode; //起点模式 @range [ 0: 起点为用户位置，当有多个起点时，多个起点表示用户的行驶方向，最后一个点为用户算路起点, 1: 表示多个起点来源于同一POI ]
    public String name; //起点POI名称
    public String poiId; //	起点POI的ID
    public String typeCode; //起点POI的TypeCode

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getMode() {
        return mode;
    }

    public void setMode(int mode) {
        this.mode = mode;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPoiId() {
        return poiId;
    }

    public void setPoiId(String poiId) {
        this.poiId = poiId;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }
}
