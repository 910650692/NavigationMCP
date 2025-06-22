package com.sgm.navi.fsa.bean;

/**
 * 获取TBT信息
 */
public class TurnInfo {
    //当前路口的位置
    private GeoPoint position;
    //下一个路口的剩余的距离
    private int remainDistance;
    //上一个路口到当前路口的距离，单位：米
    private int distanceToPreTurn;
    //当前路口到下一个路口的距离，单位：米
    private int distanceToNextTurn;
    //转向名字
    private String directionName;
    //当前路名
    private String roadName;
    //下一个路名
    private String nextRoadName;
    //是否直行
    private boolean isStraight;
    //转向类型
    private int turnKind;
    //是否为高速面板信息
    private boolean isHightway;
    //道路等级，值含义参见：3.3.4 roadLevel字段取值的含义。
    //附加说明：当该值为高速路或者城市快速路时（roadLevel = 0或者1），则说明该路段支持NOP导航
    private int roadLevel;

    public TurnInfo() {
    }

    public TurnInfo(GeoPoint position, int remainDistance, int distanceToPreTurn, int distanceToNextTurn, String directionName, String roadName, String nextRoadName, boolean isStraight, int turnKind, boolean isHightway, int roadLevel) {
        this.position = position;
        this.remainDistance = remainDistance;
        this.distanceToPreTurn = distanceToPreTurn;
        this.distanceToNextTurn = distanceToNextTurn;
        this.directionName = directionName;
        this.roadName = roadName;
        this.nextRoadName = nextRoadName;
        this.isStraight = isStraight;
        this.turnKind = turnKind;
        this.isHightway = isHightway;
        this.roadLevel = roadLevel;
    }

    public GeoPoint getPosition() {
        return position;
    }

    public void setPosition(GeoPoint position) {
        this.position = position;
    }

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public int getDistanceToPreTurn() {
        return distanceToPreTurn;
    }

    public void setDistanceToPreTurn(int distanceToPreTurn) {
        this.distanceToPreTurn = distanceToPreTurn;
    }

    public int getDistanceToNextTurn() {
        return distanceToNextTurn;
    }

    public void setDistanceToNextTurn(int distanceToNextTurn) {
        this.distanceToNextTurn = distanceToNextTurn;
    }

    public String getDirectionName() {
        return directionName;
    }

    public void setDirectionName(String directionName) {
        this.directionName = directionName;
    }

    public String getRoadName() {
        return roadName;
    }

    public void setRoadName(String roadName) {
        this.roadName = roadName;
    }

    public String getNextRoadName() {
        return nextRoadName;
    }

    public void setNextRoadName(String nextRoadName) {
        this.nextRoadName = nextRoadName;
    }

    public boolean isStraight() {
        return isStraight;
    }

    public void setStraight(boolean straight) {
        isStraight = straight;
    }

    public int getTurnKind() {
        return turnKind;
    }

    public void setTurnKind(int turnKind) {
        this.turnKind = turnKind;
    }

    public boolean isHightway() {
        return isHightway;
    }

    public void setHightway(boolean hightway) {
        isHightway = hightway;
    }

    public int getRoadLevel() {
        return roadLevel;
    }

    public void setRoadLevel(int roadLevel) {
        this.roadLevel = roadLevel;
    }

    @Override
    public String toString() {
        return "TurnInfo{" +
                "position=" + position +
                ", remainDistance=" + remainDistance +
                ", distanceToPreTurn=" + distanceToPreTurn +
                ", distanceToNextTurn=" + distanceToNextTurn +
                ", directionName=" + directionName +
                ", roadName='" + roadName + '\'' +
                ", nextRoadName='" + nextRoadName + '\'' +
                ", isStraight=" + isStraight +
                ", turnKind=" + turnKind +
                ", isHightway=" + isHightway +
                ", roadLevel=" + roadLevel +
                '}';
    }
}
