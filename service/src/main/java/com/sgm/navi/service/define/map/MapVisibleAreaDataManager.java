package com.sgm.navi.service.define.map;

import com.android.utils.ConvertUtils;
import com.android.utils.file.ParseJsonUtils;
import com.android.utils.gson.GsonUtils;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.adapter.map.MapAdapter;
import java.util.HashMap;
import java.util.Map;

public class MapVisibleAreaDataManager {

    private MapVisibleAreaDataManager mMapDataManager;
    private Map<MapVisibleAreaType,MapVisibleAreaInfo> dataMap = new HashMap<>();
    private MapAdapter mapAdapter;

    private MapVisibleAreaDataManager() {
        mMapDataManager = MapVisibleAreaDataManager.getInstance();
    }

    private void loadData() {
        String jsonPath = BuildConfig.MAP_SDK + "/buick_maparea.json";
        String json = ParseJsonUtils.parseJsonFile(jsonPath);
        MapVisibleAreaPoint point = GsonUtils.fromJson(json, MapVisibleAreaPoint.class);
        dataMap.put(MapVisibleAreaType.MAIN_AREA_CAR,point.getMap_main_car());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_NAVING,point.getMap_main_naving());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_SETTING,point.getMap_main_setting());
    }


    public void loadData(String mJsonpath) {
        String jsonPath = mJsonpath;
        String json = ParseJsonUtils.parseJsonFile(jsonPath);
        MapVisibleAreaPoint point = GsonUtils.fromJson(json, MapVisibleAreaPoint.class);
        dataMap.put(MapVisibleAreaType.MAIN_AREA_CAR,point.getMap_main_car());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_NAVING,point.getMap_main_naving());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_SETTING,point.getMap_main_setting());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_CAR_WINDOW,point.getMap_main_car_window());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_NAVING_WINDOW,point.getMap_main_naving_window());
        dataMap.put(MapVisibleAreaType.MAIN_AREA_SETTING_WINDOW,point.getMap_main_setting_window());
    }

    public static MapVisibleAreaDataManager getInstance() {
        return MapVisibleAreaDataManager.Helper.MAPDATAMAMAGER;
    }

    private static final class Helper {
        private static final MapVisibleAreaDataManager MAPDATAMAMAGER = new MapVisibleAreaDataManager();
    }

    public MapVisibleAreaInfo getDataByKey(MapVisibleAreaType mapVisibleAreaType){
        if(dataMap.containsKey(mapVisibleAreaType)){
            MapVisibleAreaInfo mapVisibleAreaInfo = dataMap.get(mapVisibleAreaType);
            if(!ConvertUtils.isEmpty(mapVisibleAreaInfo)){
                return mapVisibleAreaInfo;
            }
        }

        mapAdapter = MapAdapter.getInstance();
        MapViewParams mapSurfaceViewSizeParams = mapAdapter.getMapSurfaceParam(MapType.MAIN_SCREEN_MAIN_MAP);
        int surfaceWidth = ConvertUtils.ln2int(mapSurfaceViewSizeParams.getWidth());
        int surfaceHeight = ConvertUtils.ln2int(mapSurfaceViewSizeParams.getHeight());
        int left = surfaceWidth  / 2 ;
        int top = surfaceHeight / 2;
        return new MapVisibleAreaInfo(left,top);
    }
}
