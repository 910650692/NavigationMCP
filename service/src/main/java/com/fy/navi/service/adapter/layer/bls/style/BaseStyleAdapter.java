package com.fy.navi.service.adapter.layer.bls.style;

import com.autonavi.gbl.layer.observer.PrepareLayerParamInner;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.refix.LayerItemData;

import java.util.ArrayList;
import java.util.List;

public class BaseStyleAdapter extends PrepareLayerParamInner {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    public BaseStyleAdapter(int engineID) {
        super(engineID);
    }

    /**
     * 根据item的类型提供对应的item的json文件名
     *
     * @param item
     * @return
     */
    public String provideLayerItemStyleJson(LayerItem item) {
        return null;
    }

    /**
     * 是否需要重新组织json字符串
     *
     * @param item
     * @return
     */
    public boolean isNeedRefreshStyleJson(LayerItem item) {
        return false;
    }

    /***
     * 重新刷新json字符串
     * @param item
     * @param oldJson
     * @return
     */
    public String refreshStyleJson(LayerItem item, String oldJson) {
        return oldJson;
    }

    /**
     * 提供更新布局类型的item的数据源
     *
     * @param item
     * @return
     */
    public LayerItemData provideLayerItemData(LayerItem item) {
        return null;
    }

    /**
     * 根据item提供处理布局样式的处理器
     *
     * @param item
     * @return
     */
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        return null;
    }

    public List<CustomUpdatePair> updateTextureUpdatePair(LayerItem item) {
        return new ArrayList<>();
    }

    protected CustomUpdatePair createUpdateStylePair(String id, String style) {
        CustomUpdatePair updatePair = new CustomUpdatePair();
        updatePair.idStr = id;
        updatePair.newStyle = style;
        return updatePair;
    }

    protected CustomUpdatePair createUpdateValuePair(String id, String value) {
        CustomUpdatePair updatePair = new CustomUpdatePair();
        updatePair.idStr = id;
        updatePair.newValue = value;
        return updatePair;
    }

    protected CustomUpdatePair createUpdateValueStylePair(String id, String value, String style) {
        CustomUpdatePair updatePair = new CustomUpdatePair();
        updatePair.idStr = id;
        updatePair.newValue = value;
        updatePair.newStyle = style;
        return updatePair;
    }
}
