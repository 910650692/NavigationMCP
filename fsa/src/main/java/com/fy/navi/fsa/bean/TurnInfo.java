package com.fy.navi.fsa.bean;

public class TurnInfo {
    private GeoPoint position;
    private int remainDistance;
    private int distanceToPreTurn;
    private int distanceToNextTurn;
    private String directionName;
    private String roadName;
    private String nextRoadName;
    private boolean isStraight;
    private int turnKind;
    private boolean isHightway;
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
