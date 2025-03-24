package com.fy.navi.service.define.route;


import com.autonavi.gbl.common.path.model.RoadClass;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteTrafficIncidentDetailsInfo {
    private Coord2DDouble mPos;
    private String mTitle;
    private String mDesc;
    private short mType;
    private short mPriority;
    private short mCredibility;
    private short mSource;
    private long mID;
    private long mEventType;
    private long mLayerId;
    private long mLayerTag;
    private int mSegIndex;
    private int mLinkIndex;
    private short mTitleType;
    private short mReversed;
    private int mLane;
    private int mRoadClass;

    public RouteTrafficIncidentDetailsInfo() {
        this.mPos = new Coord2DDouble();
        this.mTitle = "";
        this.mDesc = "";
        this.mType = 0;
        this.mPriority = 0;
        this.mCredibility = 0;
        this.mSource = 0;
        this.mID = 0L;
        this.mEventType = 0L;
        this.mLayerId = 0L;
        this.mLayerTag = 0L;
        this.mSegIndex = 0;
        this.mLinkIndex = 0;
        this.mTitleType = 0;
        this.mReversed = 0;
        this.mLane = 0;
        this.mRoadClass = -1;
    }

    public RouteTrafficIncidentDetailsInfo(final Coord2DDouble posLiteObj, final String titleLiteObj, final String descLiteObj
            , final short typeLiteObj, final short priorityLiteObj, final short credibilityLiteObj, final short sourceLiteObj
            , final long idLiteObj, final long eventTypeLiteObj, final long layerIdLiteObj, final long layerTagLiteObj
            , final int segIndexLiteObj, final int linkIndexLiteObj, final short titleTypeLiteObj
            , final short reversedLiteObj, final int laneLiteObj
            ,final  @RoadClass.RoadClass1 int roadClassLiteObj) {
        this.mPos = posLiteObj;
        this.mTitle = titleLiteObj;
        this.mDesc = descLiteObj;
        this.mType = typeLiteObj;
        this.mPriority = priorityLiteObj;
        this.mCredibility = credibilityLiteObj;
        this.mSource = sourceLiteObj;
        this.mID = idLiteObj;
        this.mEventType = eventTypeLiteObj;
        this.mLayerId = layerIdLiteObj;
        this.mLayerTag = layerTagLiteObj;
        this.mSegIndex = segIndexLiteObj;
        this.mLinkIndex = linkIndexLiteObj;
        this.mTitleType = titleTypeLiteObj;
        this.mReversed = reversedLiteObj;
        this.mLane = laneLiteObj;
        this.mRoadClass = roadClassLiteObj;
    }
}
