package com.sgm.navi.fsa.bean;

public class LaneTypeInfo {
    /**
     * 车道线类型
     * 0 – 无效值
     * 1 – 普通
     * 2 – 公交车道（可能允许社会车辆通行）
     * 3 – 文字公交车道（只允许公交车通行）
     * 4 – 可变车道
     * 5 – 多成员车道
     * 6 – 文字潮汐车道
     * 7 - 前行箭头可变车道
     * 8 – 叉号可变车道
     */
    private int laneType;
    /**
     * 是否需要高亮
     */
    private boolean isBright;

    public int getLaneType() {
        return laneType;
    }

    public void setLaneType(int laneType) {
        this.laneType = laneType;
    }

    public boolean isBright() {
        return isBright;
    }

    public void setBright(boolean bright) {
        isBright = bright;
    }
}
