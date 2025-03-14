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
    private MapTypeId mapTypeId; //必要参数
    private PoiInfoEntity startPoiInfoEntity;
    private PoiInfoEntity endPoiInfoEntity;//必要参数
    private List<PoiInfoEntity> viaPoiInfoEntityList;
    private RoutePreferenceID preferenceID;
}