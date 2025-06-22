package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

/**
 * 导航信息实体类
 * @author sgm
 * @version $Revision.*$
 */
public class NaviInfoEntity {
    private long pathId;
    // 车当前所在道路的索引
    private int curSegIdx;
    // 剩余时间
    private int remainTime;
    // 红绿灯剩余数量
    private int routeRemainLightCount;

    public long getPathId() {
        return pathId;
    }

    public void setPathId(long pathId) {
        this.pathId = pathId;
    }

    public int getCurSegIdx() {
        return curSegIdx;
    }

    public void setCurSegIdx(int curSegIdx) {
        this.curSegIdx = curSegIdx;
    }

    public int getRemainTime() {
        return remainTime;
    }

    public void setRemainTime(int remainTime) {
        this.remainTime = remainTime;
    }

    public int getRouteRemainLightCount() {
        return routeRemainLightCount;
    }

    public void setRouteRemainLightCount(int routeRemainLightCount) {
        this.routeRemainLightCount = routeRemainLightCount;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviInfoEntity{" +
                "pathId=" + pathId +
                ", curSegIdx=" + curSegIdx +
                ", remainTime=" + remainTime +
                ", routeRemainLightCount=" + routeRemainLightCount +
                '}';
    }
}
