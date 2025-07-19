package com.sgm.navi.hmi.permission;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.media.projection.MediaProjectionManager;
import android.net.Uri;
import android.os.Environment;
import android.provider.Settings;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class PermissionUtils {
    private static final String TAG = PermissionUtils.class.getSimpleName();
    public static final int REQUEST_PERMISSION_CODE = 0;
    public static final int REQUEST_PERMISSION_EXTERNAL_CODE = 1;
    public static final int REQUEST_PERMISSION_OVERLAY_CODE = 2;
    public static final int REQUEST_PERMISSION_MEDIA_PROJECTION = 3;
    private static final int DENIED_PERMISSION_REQUEST_NUM = 1;
    private static final String SPEED_PERMISSION = "android.car.permission.CAR_SPEED";
    private int deniedPermissionRequestNum = 0;
    private Activity context;
    private MediaProjectionManager mProjectionManager;
    private PermissionsObserver permissionsObserver;
    private final List<String> permissionList = new ArrayList<>();
    private final List<String> deniedPermission = new ArrayList<>();
    private PermissionDialog permissionExternalDialog;
    private PermissionDialog permissionOverlayDialog;
    private final String[] permissionArray = new String[]{Manifest.permission.READ_PHONE_STATE,
            Manifest.permission.ACCESS_FINE_LOCATION,
            Manifest.permission.MANAGE_EXTERNAL_STORAGE,
            Manifest.permission.CALL_PHONE,
            Manifest.permission.GET_ACCOUNTS,
            Settings.ACTION_MANAGE_OVERLAY_PERMISSION};

    public void setPermissionsObserver(PermissionsObserver permissionsObserver) {
        this.permissionsObserver = permissionsObserver;
    }

    public boolean checkoutPermission() {
        permissionList.clear();
        deniedPermission.clear();
        for (String permission : permissionArray) {
            if (!checkoutPermission(permission)) permissionList.add(permission);
        }
        if (DeviceUtils.isCar(AppCache.getInstance().getMApplication())) {
            Logger.i(TAG, "current device type is car");
            if (!checkoutPermission(SPEED_PERMISSION)) permissionList.add(SPEED_PERMISSION);
        } else {
            Logger.i(TAG, "current device type is pad");
        }
        Logger.i(TAG, "permissionList size-> " + permissionList.size());
        return ConvertUtils.isEmpty(permissionList);
    }

    public boolean checkoutPermission(String permission) {
        Logger.i(TAG, "checkoutPermission permission-> " + permission);
        if (ConvertUtils.isEmpty(permission)) return true;
        if (ConvertUtils.equals(permission, Manifest.permission.MANAGE_EXTERNAL_STORAGE)) {
            boolean isExternalStorageManager = Environment.isExternalStorageManager();
            Logger.i(TAG, "isExternalStorageManager-> " + isExternalStorageManager);
            return isExternalStorageManager;
        }
        if (ConvertUtils.equals(permission, Settings.ACTION_MANAGE_OVERLAY_PERMISSION)) {
            return Settings.canDrawOverlays(AppCache.getInstance().getMContext());
        }
        return !ConvertUtils.isEmpty(AppCache.getInstance().getMApplication())
                && AppCache.getInstance().getMApplication().checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED;
    }

    public void requestPermission() {
        if (ConvertUtils.isEmpty(permissionList)) {
            requestDeniedPermission();
            Logger.i(TAG, "requestPermission permissionList is Empty");
        } else {
            requestPermission(permissionList.get(0));
            Logger.i(TAG, "requestPermission permissionList size-> " + permissionList.size());
        }
    }

    public void requestDeniedPermission() {
        if (ConvertUtils.isEmpty(deniedPermission)) {
            if (!ConvertUtils.isEmpty(permissionsObserver)) permissionsObserver.onPermissionsSuccess();
            return;
        }
        if (deniedPermissionRequestNum >= DENIED_PERMISSION_REQUEST_NUM) {
            if (!ConvertUtils.isEmpty(permissionsObserver)) permissionsObserver.onPermissionsFail();
            return;
        }
        if (Logger.openLog) {
            Logger.i(TAG, "requestDeniedPermission");
        }
        deniedPermissionRequestNum += 1;
        permissionList.clear();
        permissionList.addAll(deniedPermission);
        deniedPermission.clear();
        requestPermission(permissionList.get(0));
    }

    public void requestPermission(String permission) {
        permissionList.remove(permission);
        if (ConvertUtils.isEmpty(permission)) return;
        Logger.i(TAG, "current permission-> " + permission);
        if (ConvertUtils.equals(permission, Manifest.permission.MANAGE_EXTERNAL_STORAGE) && !Environment.isExternalStorageManager()) {
            Logger.i(TAG, "申请所有文件读写权限");
            requestManageExternal();
        } else if (ConvertUtils.equals(permission, Settings.ACTION_MANAGE_OVERLAY_PERMISSION)) {
            Logger.i(TAG, "申请悬浮窗权限");
            requestManageOverlay();
        } else {
            ActivityCompat.requestPermissions(StackManager.getInstance().getMainCurrentActivity(),
                    new String[]{permission}, REQUEST_PERMISSION_CODE);
        }
    }

    public void requestManageExternal() {
        Logger.i(TAG, "requestManageExternal");

        permissionExternalDialog = new PermissionDialog.Build(context)
                .setTitle("申请文件读写权限")
                .setContent("地图导航中所需权限，如果没有该权限将可能会造成地图的无法使用，点击确定跳转到权限申请界面，点击取消可能会造成地图无法使用")
                .setDialogObserver(new IBaseDialogClickListener() {

                    @Override
                    public void onCancelClick() {
                        deniedPermission.add(Manifest.permission.MANAGE_EXTERNAL_STORAGE);
                        requestPermission();
                    }

                    @Override
                    public void onCommitClick() {
                        Intent intent = new Intent(Settings.ACTION_MANAGE_ALL_FILES_ACCESS_PERMISSION);
                        ActivityCompat.startActivityForResult(context, intent, REQUEST_PERMISSION_EXTERNAL_CODE, null);
                    }
                })
                .build();
        permissionExternalDialog.show();
    }

    public void requestManageOverlay() {
        permissionOverlayDialog = new PermissionDialog.Build(context)
                .setTitle("申请悬浮窗权限")
                .setContent("地图导航中所需权限，如果没有该权限将可能会造成地图的无法使用，点击确定跳转到权限申请界面，点击取消可能会造成地图无法使用")
                .setDialogObserver(new IBaseDialogClickListener() {

                    @Override
                    public void onCancelClick() {
                        deniedPermission.add(Settings.ACTION_MANAGE_OVERLAY_PERMISSION);
                        requestPermission();
                    }

                    @Override
                    public void onCommitClick() {
                        Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
                                Uri.parse("package:" + AppCache.getInstance().getMContext().getPackageName()));
                        ActivityCompat.startActivityForResult(context, intent, REQUEST_PERMISSION_OVERLAY_CODE, null);
                    }
                })
                .build();
        permissionOverlayDialog.show();
    }

    public void onRequestPermissionsResult(@NonNull String permissions, int grantResults) {
        onRequestPermissionsResult(0, new String[]{permissions}, new int[]{grantResults});
    }

    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        if (REQUEST_PERMISSION_CODE == requestCode) {
            String permission = permissions[0];
            if (grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                Logger.i(TAG, "current permission granted success-> " + permission);
                requestPermission();
            } else {
                Logger.i(TAG, "current permission denied fail-> " + permission);
                deniedPermission.add(permission);
                requestPermission();
            }
        }
    }

    public void remove() {
        if (null != permissionExternalDialog && permissionExternalDialog.isShowing()) permissionExternalDialog.dismiss();
        permissionExternalDialog = null;
        if (null != permissionOverlayDialog && permissionOverlayDialog.isShowing()) permissionOverlayDialog.isShowing();
        permissionOverlayDialog = null;
        context = null;
        permissionsObserver = null;
    }

    private PermissionUtils() {
        context = StackManager.getInstance().getMainCurrentActivity();
    }

    public static PermissionUtils getInstance() {
        return Helper.permissionUtils;
    }

    private static final class Helper {
        @SuppressLint("StaticFieldLeak")
        private static final PermissionUtils permissionUtils = new PermissionUtils();
    }

    public interface PermissionsObserver {
        void onPermissionsSuccess();

        void onPermissionsFail();
    }
}