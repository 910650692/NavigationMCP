package com.sgm.navi.scene.ui.navi.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.databinding.ComponentTollStationBinding;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.ui.view.SkinConstraintLayout;

//收费站组件
public class ComponentTollStation extends SkinConstraintLayout {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ComponentTollStationBinding mBinding;

    public ComponentTollStation(final Context context) {
        super(context);
        initView(context);
    }

    public ComponentTollStation(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    public ComponentTollStation(final Context context,
                                @Nullable @org.jetbrains.annotations.Nullable
                                final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView(context);
    }

    /**
     * @param context context
     */
    private void initView(final Context context) {
        mBinding = ComponentTollStationBinding.inflate(LayoutInflater.from(context), this, true);
    }

    @Override
    public boolean onTouchEvent(final MotionEvent event) {
        return true;
    }

    /**
     * @param sapaItem sapaItem
     * @param isFirst isFirst
     */
    public void updateTollStationData(final SapaInfoEntity.SAPAItem sapaItem,
                                      final boolean isFirst) {
        Logger.i(TAG, "ComponentTollStation：" , sapaItem.toString() , ",isFirst：" ,
                isFirst);
        mBinding.stvTollName.setText(sapaItem.getName());
        mBinding.stvDistance.setText(TimeUtils.getInstance().getDistanceString(
                sapaItem.getRemainDist()));
    }
}
