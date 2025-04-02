package com.fy.navi.service.adapter.layer.bls.impl;

import android.view.View;

import com.fy.navi.service.define.layer.refix.LayerItemBase;

public interface ILayerItemProcessor<D extends LayerItemBase> {


    default void onFocusProcess(View rootView, D data) {
    }

    default void onNormalProcess(View rootView, D data) {
    }
}
