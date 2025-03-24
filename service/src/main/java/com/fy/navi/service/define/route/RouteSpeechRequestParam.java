package com.fy.navi.service.define.route;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
@Getter
@Setter
public class RouteSpeechRequestParam implements Serializable {
    private MapTypeId mMapTypeId; //必要参数
    private PoiInfoEntity mStartPoiInfoEntity;
    private PoiInfoEntity mEndPoiInfoEntity;//必要参数
    private List<PoiInfoEntity> mViaPoiInfoEntityList;
    private RoutePreferenceID mPreferenceID;
}