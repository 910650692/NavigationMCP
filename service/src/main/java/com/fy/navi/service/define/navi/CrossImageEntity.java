package com.fy.navi.service.define.navi;

import com.fy.navi.service.adapter.navi.NaviConstant;

/**
 * 路口大图实体类
 */
public class CrossImageEntity {
    @NaviConstant.CrossType
    private int type;             //路口大图类型:栅格/矢量/精品三维
    private int vectorType;       //矢量路口大图类型:普通/近接/混淆/环岛
    private byte[] dataBuf;       //路口大图二进制数据,栅格为jpeg,尺寸为500*320，矢量和精品3D为自定义二进制格式
    private byte[] arrowDataBuf;  //栅格路口大图二进制数据,栅格为PNG,尺寸为500*320，其他路口大图类型为空
    private boolean isOnlyVector; //特殊项目使用，路口透出多类型大图功能打开且矢量图透出时生效:是否只有矢量图显示 false : 不是， true : 是
    private long distance;        //当前位置距路口大图距离

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getVectorType() {
        return vectorType;
    }

    public void setVectorType(int vectorType) {
        this.vectorType = vectorType;
    }

    public byte[] getDataBuf() {
        return dataBuf;
    }

    public void setDataBuf(byte[] dataBuf) {
        this.dataBuf = dataBuf;
    }

    public byte[] getArrowDataBuf() {
        return arrowDataBuf;
    }

    public void setArrowDataBuf(byte[] arrowDataBuf) {
        this.arrowDataBuf = arrowDataBuf;
    }

    public boolean isOnlyVector() {
        return isOnlyVector;
    }

    public void setOnlyVector(boolean onlyVector) {
        isOnlyVector = onlyVector;
    }

    public long getDistance() {
        return distance;
    }

    public void setDistance(long distance) {
        this.distance = distance;
    }
}
