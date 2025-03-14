package com.fy.navi.service.greendao.setting;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.greendao.CommonSetting;
import com.fy.navi.service.greendao.CommonSettingDao;
import com.fy.navi.service.greendao.DaoMaster;

import org.greenrobot.greendao.database.Database;

import java.util.Date;
import java.util.List;

/**
 * @Author: fenghong0322
 * @Description: 设置项数据库管理
 * @CreateDate: 2025-01-08
 */
public class SettingManager {
    private static final String TAG = SettingManager.class.getSimpleName();
    private static final String DB_NAME = "setting.db";
    private static SettingManager mDBHelper;
    private NaviSettingDao mSettingDao;

    /**
     * Get instance.
     */
    public static SettingManager getInstance() {
        if (mDBHelper == null) {
            synchronized (SettingManager.class) {
                if (mDBHelper == null) {
                    mDBHelper = new SettingManager();
                }
                return mDBHelper;
            }
        }
        return mDBHelper;
    }

    /**
     * Init database and cloud data.
     */
    public void init() {
        // 数据库对象
        DaoMaster.DevOpenHelper helper = new DaoMaster.DevOpenHelper(AppContext.mContext, DB_NAME);
        Database db = helper.getWritableDb();
        DaoMaster daoMaster = new DaoMaster(db);
        mSettingDao = daoMaster.newSession().getNaviSettingDao();
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
        List<NaviSetting> saveData = mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.Name.eq(key))
                .list();
        return !saveData.isEmpty();
    }

    /**
     * 修改或者更新设置项数据
     * @param key
     * @param value
     */
    public void insertOrReplace(String key, String value) {
        Logger.d(TAG, "insertOrReplace key = " + key + "  value = " + value);
        NaviSetting setting = new NaviSetting();
        setting.setName(key);
        setting.setValue(value);
        setting.setUpdateTime(new Date());
        mSettingDao.insertOrReplace(setting);
    }

    /**
     * 通过key查找其对应value
     */
    public String getValueByKey(String key) {
        NaviSetting naviSetting = mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.Name.eq(key))
                .unique();
        if (naviSetting != null) {
            return naviSetting.getValue();
        } else {
            return "";
        }
    }

    /**
     * 查询全部设置数据.
     * @return all data list,String
     */
    private List<NaviSetting> loadAll() {
        List<NaviSetting> settingList = mSettingDao.loadAll();
        return settingList;
    }

    /**
     * 更新设置项数据
     */
    private void update(String key, String value) {
        NaviSetting unique = mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.Name.eq(key))
                .unique();
        unique.setValue(value);
        mSettingDao.update(unique);
    }

    /**
     * 删除某个设置项数据
     */
    public void deleteValue(String key) {
        mSettingDao.queryBuilder()
                .where(NaviSettingDao.Properties.Name.eq(key))
                .buildDelete()
                .executeDeleteWithoutDetachingEntities();
    }

    /**
     * 清空全部数据
     */
    public void deleteAll() {
        mSettingDao.deleteAll();
    }

}
