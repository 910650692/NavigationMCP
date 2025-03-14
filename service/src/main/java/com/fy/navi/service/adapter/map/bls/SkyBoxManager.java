package com.fy.navi.service.adapter.map.bls;

import com.android.utils.file.FileUtils;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.model.MapSkyboxParam;
import com.autonavi.gbl.util.model.BinaryStream;

/**
 * Author: QiuYaWei
 * Date: 2025/2/23
 * Description: [在这里描述文件功能]
 */
public class SkyBoxManager {
    private SkyBoxManager() {
    }

    private static final class InstanceHolder {
        private static final SkyBoxManager instance = new SkyBoxManager();
    }

    public static SkyBoxManager getInstance() {
        return InstanceHolder.instance;
    }

    public void initSkyBox(MapView mapView) {
        //获取天空盒子纹理数据
        BinaryStream binaryStream = null;
        byte[] skyboxDay = FileUtils.getInstance().getAssetFileContent("blRes/MapAsset/skybox_day.data");
        binaryStream = new BinaryStream(skyboxDay);

        //设置天空盒子
        MapSkyboxParam mapSkyboxParam = new MapSkyboxParam();
        mapSkyboxParam.isOn = true; // 是否开启skybox，默认为true，非必须设置
        mapSkyboxParam.is3DRes = false; // 是否使用3D资源，*.data资源为2D天空卷轴，传false
        mapSkyboxParam.DataBuff = binaryStream;
        mapSkyboxParam.frogColor = 0xFFDC143C; //雾气颜色，ARGB，默认为0xEFF4FFFF，非必须设置
        mapView.getOperatorBusiness().setMapSkyboxParam(mapSkyboxParam);
    }

    public void updateSkyBox(MapView mapView, boolean isNight) {
        String assetFileRelPath = isNight ? "blRes/MapAsset/skybox_night.data" : "blRes/MapAsset/skybox_day.data";
        //获取天空盒子纹理数据
        BinaryStream binaryStream = null;
        byte[] skyboxDay = FileUtils.getInstance().getAssetFileContent(assetFileRelPath);
        binaryStream = new BinaryStream(skyboxDay);

        //设置天空盒子
        MapSkyboxParam mapSkyboxParam = new MapSkyboxParam();
        mapSkyboxParam.isOn = true; // 是否开启skybox，默认为true，非必须设置
        mapSkyboxParam.is3DRes = false; // 是否使用3D资源，*.data资源为2D天空卷轴，传false
        mapSkyboxParam.DataBuff = binaryStream;
        mapSkyboxParam.frogColor = 0xFFDC143C; //雾气颜色，ARGB，默认为0xEFF4FFFF，非必须设置
        mapView.getOperatorBusiness().setMapSkyboxParam(mapSkyboxParam);
    }

}
