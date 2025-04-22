package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.NavGuidanceSimulationViewBinding;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.view.SkinRelativeLayout;


public class GuidanceSimulationView extends SkinRelativeLayout implements View.OnClickListener {
    private static final String TAG = "GuidanceSimulationView";
    private NavGuidanceSimulationViewBinding mBinding;
    private boolean startSim = true;

    /**
     * 模拟导航速度
     */
    private int mCurrentSeed = SIM_NAVI_SPEED[1];
    protected static final int[] SIM_NAVI_SPEED = {10, 40, 80, 120, 200, 400, 800, 1600};

    public GuidanceSimulationView(Context context) {
        super(context);
        initView(context);
    }

    public GuidanceSimulationView(Context context, AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    public GuidanceSimulationView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView(context);
    }

    private void initView(Context context) {
        LayoutInflater inflater =
                (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        View view = inflater.inflate(R.layout.nav_guidance_simulation_view, this);
        mBinding = NavGuidanceSimulationViewBinding.bind(view);
        mBinding.switchSimNavi.setOnClickListener(this);
        mBinding.switchSimSpeed.setOnClickListener(this);
        mBinding.simSpeedReduce.setOnClickListener(this);
    }

    /**
     * Called when a view has been clicked.
     *
     * @param v The view that was clicked.
     */
    @Override
    public void onClick(View v) {
        if (v.getId() == R.id.switch_sim_navi) {
            startSim = !startSim;
            mBinding.switchSimNavi.setBackgroundResource(startSim ? R.drawable.pause_sim_navi : R.drawable.start_sim_navi);
            if (startSim) {
                NaviPackage.getInstance().resumeNavi();
            } else {
                NaviPackage.getInstance().pauseNavi();
            }
        } else if (v.getId() == R.id.switch_sim_speed) {
            String str = setSimulateCarSpeed();
            mBinding.switchSimSpeed.setText(str);
        } else if (v.getId() == R.id.sim_speed_reduce) {
            String str = reduceSimulateSpeed();
            mBinding.switchSimSpeed.setText(str);
        }
    }

    public String setSimulateCarSpeed() {
        for (int i = 0; i < SIM_NAVI_SPEED.length; i++) {
            if (mCurrentSeed == SIM_NAVI_SPEED[i]) {
                if (i == SIM_NAVI_SPEED.length - 1) {
                    mCurrentSeed = SIM_NAVI_SPEED[0];
                } else {
                    mCurrentSeed = SIM_NAVI_SPEED[i + 1];
                }
                break;
            }
        }
        NaviPackage.getInstance().setSimulationSpeed(mCurrentSeed);
        return mCurrentSeed + "km/h";
    }

    public String reduceSimulateSpeed() {
        for (int i = 0; i < SIM_NAVI_SPEED.length; i++) {
            if (mCurrentSeed == SIM_NAVI_SPEED[i]) {
                if (i == 0) {
                    mCurrentSeed = SIM_NAVI_SPEED[SIM_NAVI_SPEED.length - 1];
                } else {
                    mCurrentSeed = SIM_NAVI_SPEED[i - 1];
                }
                break;
            }
        }
        NaviPackage.getInstance().setSimulationSpeed(mCurrentSeed);
        return mCurrentSeed + "km/h";
    }
}
