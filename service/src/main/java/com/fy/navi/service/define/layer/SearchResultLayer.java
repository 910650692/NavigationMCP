package com.fy.navi.service.define.layer;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/22
 */
public class SearchResultLayer {
    private ArrayList<ParentPoint> parentPoints = new ArrayList<>();
    private ArrayList<ChildPoint> childPoints = new ArrayList<>();

    public ArrayList<ParentPoint> getParentPoints() {
        return parentPoints;
    }

    public void setParentPoints(ArrayList<ParentPoint> parentPoints) {
        this.parentPoints = parentPoints;
    }

    public ArrayList<ChildPoint> getChildPoints() {
        return childPoints;
    }

    public void setChildPoints(ArrayList<ChildPoint> childPoints) {
        this.childPoints = childPoints;
    }

    public static class ParentPoint{
        public String id;
        public String poiName;
        public int typeCode = 0;
        public int index = 0;
        public int poiType = 0;
        public GeoPoint mPos3D = new GeoPoint();

        public ParentPoint() {
        }

        public ParentPoint(int typeCode, String poiName, int index, String id, int poiType) {
            this.typeCode = typeCode;
            this.poiName = poiName;
            this.index = index;
            this.id = id;
            this.poiType = poiType;
        }

        public ParentPoint(int typeCode, String poiName, int index, String id, int poiType, GeoPoint mPos3D) {
            this.typeCode = typeCode;
            this.poiName = poiName;
            this.index = index;
            this.id = id;
            this.poiType = poiType;
            this.mPos3D = mPos3D;
        }

        @Override
        public String toString() {
            return "ParentPoint{" +
                    "id='" + id + '\'' +
                    ", poiName='" + poiName + '\'' +
                    ", typeCode=" + typeCode +
                    ", index=" + index +
                    ", poiType=" + poiType +
                    ", displayPoint=" + mPos3D +
                    '}';
        }
    }

    public static class ChildPoint{
        public String mTypeCode = "";
        public int childType;
        public String shortName;
        public GeoPoint mPos3D;
        public String mPid;
        public ChildPoint() {
        }

        public ChildPoint(String typeCode, int childType, String shortName, GeoPoint mPos3D, String pid) {
            this.mTypeCode = typeCode;
            this.childType = childType;
            this.shortName = shortName;
            this.mPos3D = mPos3D;
            this.mPid = pid;
        }
    }

    @Override
    public String toString() {
        return "SearchResultLayer{" +
                "parentPoints=" + parentPoints +
                "childPoints=" + childPoints +
                '}';
    }
}
