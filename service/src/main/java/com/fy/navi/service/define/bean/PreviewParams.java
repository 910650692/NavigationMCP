package com.fy.navi.service.define.bean;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/24
 */
public class PreviewParams implements Serializable {
    private List<PointD> points = new ArrayList<>();
    private RectDouble mapBound = new RectDouble();
    private boolean bUseRect = true;
    /*** 路线全览还是搜索结果全览 true 路线全览/false 搜索结果全览**/
    private boolean isRouteLine = true;
    private int screenLeft = 100;
    private int screenTop = 100;
    private int screenRight = 100;
    private int screenBottom = 100;
    private int leftOfMap;
    private int topOfMap;


    public PreviewParams() {
    }

    public PreviewParams(List<PointD> points, RectDouble mapBound) {
        this.points = points;
        this.mapBound = mapBound;
    }

    public PreviewParams(List<PointD> points, RectDouble mapBound, boolean bUseRect, boolean isRouteLine) {
        this.points = points;
        this.mapBound = mapBound;
        this.bUseRect = bUseRect;
        this.isRouteLine = isRouteLine;
    }

    public List<PointD> getPoints() {
        return points;
    }

    public void setPoints(List<PointD> points) {
        this.points = points;
    }

    public RectDouble getMapBound() {
        return mapBound;
    }

    public void setMapBound(RectDouble mapBound) {
        this.mapBound = mapBound;
    }

    public boolean isbUseRect() {
        return bUseRect;
    }

    public void setbUseRect(boolean bUseRect) {
        this.bUseRect = bUseRect;
    }

    public boolean isRouteLine() {
        return isRouteLine;
    }

    public void setRouteLine(boolean routeLine) {
        isRouteLine = routeLine;
    }

    public int getScreenLeft() {
        return screenLeft;
    }

    public void setScreenLeft(int screenLeft) {
        this.screenLeft = screenLeft;
    }

    public int getScreenTop() {
        return screenTop;
    }

    public void setScreenTop(int screenTop) {
        this.screenTop = screenTop;
    }

    public int getScreenRight() {
        return screenRight;
    }

    public void setScreenRight(int screenRight) {
        this.screenRight = screenRight;
    }

    public int getScreenBottom() {
        return screenBottom;
    }

    public void setScreenBottom(int screenBottom) {
        this.screenBottom = screenBottom;
    }

    public int getLeftOfMap() {
        return leftOfMap;
    }

    public void setLeftOfMap(int leftOfMap) {
        this.leftOfMap = leftOfMap;
    }

    public int getTopOfMap() {
        return topOfMap;
    }

    public void setTopOfMap(int topOfMap) {
        this.topOfMap = topOfMap;
    }

    @Override
    public String toString() {
        return "PreviewParams{" +
                "points=" + points +
                ", mapBound=" + mapBound +
                ", bUseRect=" + bUseRect +
                ", isRouteLine=" + isRouteLine +
                ", screenLeft=" + screenLeft +
                ", screenTop=" + screenTop +
                ", screenRight=" + screenRight +
                ", screenBottom=" + screenBottom +
                '}';
    }

    public static class RectDouble implements Serializable {
        public double left = 0.0;
        public double right = 0.0;
        public double top = 0.0;
        public double bottom = 0.0;

        public RectDouble() {
        }

        public RectDouble(double left, double right, double top, double bottom) {
            this.left = left;
            this.right = right;
            this.top = top;
            this.bottom = bottom;
        }

        @Override
        public String toString() {
            return "RectDouble{" +
                    "left=" + left +
                    ", right=" + right +
                    ", top=" + top +
                    ", bottom=" + bottom +
                    '}';
        }
    }

    public static class PointD implements Serializable {
        public double x;
        public double y;

        public PointD() {

        }

        public PointD(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }
}
