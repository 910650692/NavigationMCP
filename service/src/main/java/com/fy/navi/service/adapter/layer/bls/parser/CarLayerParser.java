package com.fy.navi.service.adapter.layer.bls.parser;

import android.annotation.SuppressLint;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.file.ParseJsonUtils;
import com.autonavi.gbl.layer.model.BizCarType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.util.model.BinaryStream;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * @Description 车标解析类
 * @Author lvww
 * @date 2024/12/12
 */
public class CarLayerParser {
    private final int THREE_D_CAR_STYLE_RES_ID = 0x10002;
    private JSONObject mJsonObject;
    private String m3DModelName;

    public CarLayerParser() {
        try {
            if (mJsonObject == null) {
                mJsonObject = new JSONObject(ParseJsonUtils.parseJsonFile("bls/style1/car_logo.json"));
            }
        } catch (JSONException ignored) {
        }
    }

    /**
     * 获取车标图层MarkerInfo.
     *
     * @param carType 车标业务类型
     * @return 车标MarkerInfo
     */
    public String get3DCarLayer(@BizCarType.BizCarType1 int carType) {
        return ParseJsonUtils.getStyleBeanJson(mJsonObject, "car_marker_info_3d");
    }

    /**
     * 获取车标图层纹理.
     *
     * @param layerItemInfo 图层信息
     * @param carType       车标类型
     * @return 车标图层纹理Json文件
     */
    @SuppressLint("SwitchIntDef")
    public String getCarLayer(String layerItemInfo, @BizCarType.BizCarType1 int carType) {
        String strStyleJson;
        int carMode = ConvertUtils.str2Int(layerItemInfo);
        switch (carType) {
            case BizCarType.BizCarTypeGuide -> strStyleJson = "car_guidance_logo";// 导航车标 2001
            case BizCarType.BizCarTypeCruise -> strStyleJson = "car_cruise_logo";// 巡航车标 2002
            case BizCarType.BizCarTypeSearch -> strStyleJson = "car_normal_logo";// 搜索结果车标 2003
            case BizCarType.BizCarTypeRoute -> strStyleJson = "car_normal_logo";// 路径规划车标 2004
            case BizCarType.BizCarTypeEagleEye -> {
                carMode = CarMode.AUTO_UNKNOWN_ERROR;
                strStyleJson = "car_eagle_eyes_logo_2d";
            }
            default -> strStyleJson = "car_normal_logo";// 后续需要增加位置类型车标
        }
        return ParseJsonUtils.getStyleBeanJson(mJsonObject, getCarModelJsonName(carMode, strStyleJson));
    }

    /**
     * 加载车标模型ID.
     *
     * @param mapView 车标所在的视图
     * @return 车标Id
     */
    public int get3DCarModelMarkerId(MapView mapView, String str3DModelId) {
        int retValue = -1;
        String carModel3dLayerPath = "bls/style1/car_3d_logo/1/" + str3DModelId + ".dat";
        if (ConvertUtils.isEmpty(m3DModelName)) {
            mapView.addLayer3DModel(createLayer3DModel(carModel3dLayerPath));
        } else if (!ConvertUtils.equals(m3DModelName, carModel3dLayerPath)) {
            mapView.updateLayer3DModel(createLayer3DModel(carModel3dLayerPath));
        }
        m3DModelName = carModel3dLayerPath;
        retValue = THREE_D_CAR_STYLE_RES_ID;
        return retValue;
    }

    /**
     * 根据车标业务类型确定车标模式的资源.
     *
     * @param carModel 车标模式
     * @param jsonName 业务类型所对应的Json Key
     * @return 车标模式的Layer Name
     */
    private String getCarModelJsonName(int carModel, String jsonName) {
        return switch (carModel) {
            case CarMode.CarMode2D -> jsonName + "_2d";
            case CarMode.CarMode3D -> jsonName + "_3d";
            case CarMode.CarModeSkeleton -> jsonName + "_skeleton";
            case CarMode.CarModeSpeed -> jsonName + "_speed";
            default -> jsonName;
        };
    }

    /**
     * 构建车标建模对象.
     *
     * @param modelName 车标建模的名字
     * @return 建模对象
     */
    private Layer3DModel createLayer3DModel(String modelName) {
        byte[] buffer = FileUtils.getInstance().getAssetFileContent(modelName);
        Layer3DModel modelParam = new Layer3DModel();
        modelParam.resourceID = THREE_D_CAR_STYLE_RES_ID;
        modelParam.dataBuff = new BinaryStream(buffer);
        return modelParam;
    }
}