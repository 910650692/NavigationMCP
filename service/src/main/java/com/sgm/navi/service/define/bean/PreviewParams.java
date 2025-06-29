package com.sgm.navi.service.define.bean;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/24
 */
public class PreviewParams implements Parcelable {
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

    protected PreviewParams(Parcel in) {
        bUseRect = in.readByte() != 0;
        isRouteLine = in.readByte() != 0;
        screenLeft = in.readInt();
        screenTop = in.readInt();
        screenRight = in.readInt();
        screenBottom = in.readInt();
        leftOfMap = in.readInt();
        topOfMap = in.readInt();
    }

    public static final Creator<PreviewParams> CREATOR = new Creator<PreviewParams>() {
        @Override
        public PreviewParams createFromParcel(Parcel in) {
            return new PreviewParams(in);
        }

        @Override
        public PreviewParams[] newArray(int size) {
            return new PreviewParams[size];
        }
    };

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

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeByte((byte) (bUseRect ? 1 : 0));
        dest.writeByte((byte) (isRouteLine ? 1 : 0));
        dest.writeInt(screenLeft);
        dest.writeInt(screenTop);
        dest.writeInt(screenRight);
        dest.writeInt(screenBottom);
        dest.writeInt(leftOfMap);
        dest.writeInt(topOfMap);
    }

    public static class RectDouble implements Parcelable {
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

        protected RectDouble(Parcel in) {
            left = in.readDouble();
            right = in.readDouble();
            top = in.readDouble();
            bottom = in.readDouble();
        }

        public static final Creator<RectDouble> CREATOR = new Creator<RectDouble>() {
            @Override
            public RectDouble createFromParcel(Parcel in) {
                return new RectDouble(in);
            }

            @Override
            public RectDouble[] newArray(int size) {
                return new RectDouble[size];
            }
        };

        @Override
        public String toString() {
            return "RectDouble{" +
                    "left=" + left +
                    ", right=" + right +
                    ", top=" + top +
                    ", bottom=" + bottom +
                    '}';
        }

        @Override
        public int describeContents() {
            return 0;
        }

        @Override
        public void writeToParcel(@NonNull Parcel dest, int flags) {
            dest.writeDouble(left);
            dest.writeDouble(right);
            dest.writeDouble(top);
            dest.writeDouble(bottom);
        }
    }

    public static class PointD implements Parcelable {
        public double x;
        public double y;

        public PointD() {

        }

        public PointD(double x, double y) {
            this.x = x;
            this.y = y;
        }

        protected PointD(Parcel in) {
            x = in.readDouble();
            y = in.readDouble();
        }

        public static final Creator<PointD> CREATOR = new Creator<PointD>() {
            @Override
            public PointD createFromParcel(Parcel in) {
                return new PointD(in);
            }

            @Override
            public PointD[] newArray(int size) {
                return new PointD[size];
            }
        };

        @Override
        public int describeContents() {
            return 0;
        }

        @Override
        public void writeToParcel(@NonNull Parcel dest, int flags) {
            dest.writeDouble(x);
            dest.writeDouble(y);
        }
    }
}
