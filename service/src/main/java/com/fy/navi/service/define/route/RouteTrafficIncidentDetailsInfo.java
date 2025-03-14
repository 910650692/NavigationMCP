package com.fy.navi.service.define.route;

import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.path.model.RoadClass;

public class RouteTrafficIncidentDetailsInfo {
    public com.autonavi.gbl.common.model.Coord2DDouble pos;
    public String title;
    public String desc;
    public short type;
    public short priority;
    public short credibility;
    public short source;
    public long ID;
    public long eventType;
    public long layerId;
    public long layerTag;
    public int segIndex;
    public int linkIndex;
    public short titleType;
    public short reversed;
    public int lane;
    public int roadClass;

    public RouteTrafficIncidentDetailsInfo() {
        this.pos = new com.autonavi.gbl.common.model.Coord2DDouble();
        this.title = "";
        this.desc = "";
        this.type = 0;
        this.priority = 0;
        this.credibility = 0;
        this.source = 0;
        this.ID = 0L;
        this.eventType = 0L;
        this.layerId = 0L;
        this.layerTag = 0L;
        this.segIndex = 0;
        this.linkIndex = 0;
        this.titleType = 0;
        this.reversed = 0;
        this.lane = 0;
        this.roadClass = -1;
    }

    public RouteTrafficIncidentDetailsInfo(Coord2DDouble posLiteObj, String titleLiteObj, String descLiteObj, short typeLiteObj, short priorityLiteObj, short credibilityLiteObj, short sourceLiteObj, long IDLiteObj, long eventTypeLiteObj, long layerIdLiteObj, long layerTagLiteObj, int segIndexLiteObj, int linkIndexLiteObj, short titleTypeLiteObj, short reversedLiteObj, int laneLiteObj, @RoadClass.RoadClass1 int roadClassLiteObj) {
        this.pos = posLiteObj;
        this.title = titleLiteObj;
        this.desc = descLiteObj;
        this.type = typeLiteObj;
        this.priority = priorityLiteObj;
        this.credibility = credibilityLiteObj;
        this.source = sourceLiteObj;
        this.ID = IDLiteObj;
        this.eventType = eventTypeLiteObj;
        this.layerId = layerIdLiteObj;
        this.layerTag = layerTagLiteObj;
        this.segIndex = segIndexLiteObj;
        this.linkIndex = linkIndexLiteObj;
        this.titleType = titleTypeLiteObj;
        this.reversed = reversedLiteObj;
        this.lane = laneLiteObj;
        this.roadClass = roadClassLiteObj;
    }
}
