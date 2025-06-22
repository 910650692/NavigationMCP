package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

/**
 * 导航路线切换实体类
 * @author sgm
 * @version $Revision.*$
 */
public class NaviExchangeEntity {

    // 切换类型ExchangeType
    private int exchangeType;
    // 新路线
    private NewRoute newRoute;

    public NaviExchangeEntity() {
    }

    public NaviExchangeEntity(int exchangeType, NewRoute newRoute) {
        this.exchangeType = exchangeType;
        this.newRoute = newRoute;
    }

    public static class ExchangeType {
        // 当前路线已路过此道路
        public static final int ROAD_HAS_PASSED = 0;
        // 没有合理线路
        public static final int NO_ROUTE = 1;
        // 当前线路不包含此道路
        public static final int ROAD_NOT_IN_CURRENT_ROUTE = 2;
        // 正常切换
        public static final int NORMAL_EXCHANGE = 3;
        // 当前线路包含此道路
        public static final int ROAD_IN_CURRENT_ROUTE = 4;
    }

    /**
     * 新路线实体类
     */
    public static class NewRoute {
        // 切换路线的pathId
        private long pathId;
        // 和原先路线的时间对比，如果是正数，表示增加时间，如果是负数，表示减少时间
        private int comePareTime;
        // 和原先路线的交通信号灯对比，如果是正数，表示增加交通信号灯，如果是负数，表示减少交通信号灯
        private int comePareTrafficLights;

        public long getPathId() {
            return pathId;
        }

        public void setPathId(long pathId) {
            this.pathId = pathId;
        }

        public int getComePareTime() {
            return comePareTime;
        }

        public void setComePareTime(int comePareTime) {
            this.comePareTime = comePareTime;
        }

        public int getComePareTrafficLights() {
            return comePareTrafficLights;
        }

        public void setComePareTrafficLights(int comePareTrafficLights) {
            this.comePareTrafficLights = comePareTrafficLights;
        }

        @NonNull
        @Override
        public String toString() {
            return "NewRoute{" +
                    "pathId=" + pathId +
                    ", comePareTime=" + comePareTime +
                    ", comePareTrafficLights=" + comePareTrafficLights +
                    '}';
        }
    }

    public int getExchangeType() {
        return exchangeType;
    }

    public void setExchangeType(int exchangeType) {
        this.exchangeType = exchangeType;
    }

    public NewRoute getNewRoute() {
        return newRoute;
    }

    public void setNewRoute(NewRoute newRoute) {
        this.newRoute = newRoute;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviExchangeEntity{" +
                "exchangeType=" + exchangeType +
                ", newRoute=" + newRoute +
                '}';
    }
}
