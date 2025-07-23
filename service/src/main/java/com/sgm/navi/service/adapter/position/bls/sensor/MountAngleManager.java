package com.sgm.navi.service.adapter.position.bls.sensor;


import com.sgm.navi.service.BuildConfig;

public class MountAngleManager {
    private static class Holder {
        private static MountAngleManager mInstance = new MountAngleManager();
    }

    public MountAngleInfo getMountAngleInfo() {
        return mInfo;
    }

    MountAngleInfo mInfo;

    private MountAngleManager() {
        MountAngleInfo info = new MountAngleInfo();
        // TODO: 2025/2/25 此处后期可做车型区分
        info.yaw = BuildConfig.MOUNT_ANGLE_YAW;
        info.pitch = BuildConfig.MOUNT_ANGLE_PITCH;
        info.roll = 0.0f;
        info.x = 0; //原x轴
        info.y = 1; //原y轴
        info.z = 2; //原z轴
        mInfo = info;
    }

    public static MountAngleManager getInstance() {
        return Holder.mInstance;
    }

    public static class MountAngleInfo {
        //Z轴正方形指向车顶上方，因为绕着Z轴滚转时车辆是在做偏航的动作，所以也称为偏航轴-Yaw
        public float yaw;
        //X轴正方向指向车辆右侧，因为绕着X轴转动时车辆是在做俯仰的动作，所以也称为俯仰轴-Pitch
        public float pitch;
        //Y轴正方向指向车辆前方，因为绕着Y轴转动时车辆是在做滚转的动作，所以也称为滚转轴-Roll
        public float roll;
        public int x;
        public int y;
        public int z;

        @Override
        public String toString() {
            return "MountAngleInfo{" +
                    "yaw=" + yaw +
                    ", pitch=" + pitch +
                    ", roll=" + roll +
                    ", x=" + x +
                    ", y=" + y +
                    ", z=" + z +
                    '}';
        }
    }
}
