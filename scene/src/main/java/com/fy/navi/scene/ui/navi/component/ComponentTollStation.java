package com.fy.navi.scene.ui.navi.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.android.utils.StringUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ComponentTollStationBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.ui.view.SkinConstraintLayout;
import com.fy.navi.ui.view.SkinRelativeLayout;

//收费站组件
public class ComponentTollStation extends SkinConstraintLayout {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ComponentTollStationBinding mBinding;

    public ComponentTollStation(Context context) {
        super(context);
        initView(context);
    }

    public ComponentTollStation(Context context, AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    public ComponentTollStation(Context context, @Nullable @org.jetbrains.annotations.Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView(context);
    }

    private void initView(Context context) {
        mBinding = ComponentTollStationBinding.inflate(LayoutInflater.from(context), this, true);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        return true;
    }

    public void updateTollStationData(SapaInfoEntity.SAPAItem sapaItem, boolean isFirst) {
        Logger.i(TAG, "ComponentTollStation：" + sapaItem.toString() + ",isFirst：" + isFirst);
        mBinding.stvTollName.setText(sapaItem.getName());
        mBinding.stvDistance.setText(TimeUtils.getInstance().getDistanceString(sapaItem.getRemainDist()));
    }
}
