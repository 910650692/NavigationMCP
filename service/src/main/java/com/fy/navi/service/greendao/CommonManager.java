package com.fy.navi.service.greendao;

import androidx.annotation.WorkerThread;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AppContext;

import org.greenrobot.greendao.database.Database;

import java.util.Date;
import java.util.List;

/**
 * @Author: fenghong0322
 * @Description: 公用项数据库管理
 * @CreateDate: 2025-01-08
 */
public class CommonManager {
    private static final String TAG = CommonManager.class.getSimpleName();
    private static final String DB_NAME = "common.db";
    private static CommonManager mCommonManager;
    private CommonSettingDao mCommonSettingDao;
    private boolean isInit = false;
    private CommonManager() {

    }
    /**
     * Get instance.
     */
    public static CommonManager getInstance() {
        if (mCommonManager == null) {
            synchronized (CommonManager.class) {
                if (mCommonManager == null) {
                    mCommonManager = new CommonManager();
                }
                return mCommonManager;
            }
        }
        return mCommonManager;
    }

    /**
     * Init database and cloud data.
     */
    public void init() {
        if (!isInit) {
            // 数据库对象
            DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(AppContext.mContext, DB_NAME);
            Database db = helper.getWritableDb();
            DaoMaster daoMaster = new DaoMaster(db);
            mCommonSettingDao = daoMaster.newSession().getCommonSettingDao();
            isInit = true;
            Logger.i(TAG, "Database init success!");
        } else {
            Logger.i(TAG, "Database had Initialized!");
        }
    }

    /**
     * Use key and value for data settings.
     * @param key save key
     * @param value save value
     */
    public void insertValue(String key, String value) {
        Logger.d(TAG, "key = " + key + "  value = " + value);
        if (searchFromDb(key)) {
            Logger.d(TAG, "update");
            update(key, value);
        } else {
            Logger.d(TAG, "insertOrReplace");
            insertOrReplace(key, value);
        }
    }

    private boolean searchFromDb(String key) {
        List<CommonSetting> saveData = mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.Name.eq(key))
                .list();
        return !saveData.isEmpty();
    }

    /**
     * 修改或者更新设置项数据
     * @param key
     * @param value
     */
    @WorkerThread
    public void insertOrReplace(String key, String value) {
        ThreadManager.getInstance().asyncDelay(() -> {
            CommonSetting setting = new CommonSetting();
            setting.setName(key);
            setting.setValue(value);
            setting.setUpdateTime(new Date());
            mCommonSettingDao.insertOrReplace(setting);
        },0);
    }

    /**
     * 通过key查找其对应value
     */
    public String getValueByKey(String key) {
        CommonSetting commonSetting = mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.Name.eq(key))
                .unique();
        if (commonSetting != null) {
            return commonSetting.getValue();
        } else {
            return "";
        }
    }

    /**
     * 更新数据
     */
    private void update(String key, String value) {
        CommonSetting unique = mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.Name.eq(key))
                .unique();
        unique.setValue(value);
        mCommonSettingDao.update(unique);
    }

    /**
     * 删除某个数据
     */
    public void deleteValue(String key) {
        mCommonSettingDao.queryBuilder()
                .where(CommonSettingDao.Properties.Name.eq(key))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 清空全部数据
     */
    public void deleteAll() {
        mCommonSettingDao.deleteAll();
    }

}
