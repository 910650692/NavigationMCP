package com.android.utils.gson;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;

/**
 * @Introduce: .
 * @Author: lvww
 * @Date: 2023/11/21
 * @Description:
 */
public class GsonUtils {
    private static String[] type = new String[]{"String", "Boolean", "Byte", "Character", "Short", "Integer", "Long", "Float", "Double"};
    private static final int LAST_INDEX = 6;
    private static final Gson gsonBuild = new GsonBuilder()
            .serializeNulls()
            .disableHtmlEscaping()
//            .setDateFormat("yyyy-MM-dd")
//            .setPrettyPrinting()
            .create();

    /**
     * 序列化.
     *
     * @param object
     * @return 实体转json
     */
    public static final String toJson(Object object) {
        ConvertUtils.checkParam("toJson", object, gsonBuild);
        return gsonBuild.toJson(object, object.getClass());
    }

    /**
     * 序列化.
     *
     * @param t
     * @param <T>
     * @return 实体转json
     */
    public static final <T> String toJson(T[] t) {
        ConvertUtils.checkParam("toJson", t, gsonBuild);
        return gsonBuild.toJson(t, t.getClass());
    }

    /**
     * 反序列化.
     *
     * @param json     json字符
     * @param classOfT 映射体
     * @return json转换为实体
     */
    public static final <T> T fromJson(String json, Class<T> classOfT) {
        ConvertUtils.checkParam("fromJson", json, gsonBuild);
        return gsonBuild.fromJson(json, classOfT);
    }

    /**
     * 反序列化.
     *
     * @param json     json字符
     * @param classOfT 映射体
     * @return json转换为实体
     */
    public static final <T> T fromJsonV2(String json, Class<T> classOfT) {
        return gsonBuild.fromJson(json, classOfT);
    }

    /**
     * 反序列化 转集合.
     *
     * @param json
     * @param <T>
     * @return 集合实体
     */
    public static final <T> List<T> fromJson2List(String json, Class<T> clazz) {
        ConvertUtils.checkParam("fromJson2List", json, gsonBuild);
        return fromJson2List(json, new TypeToken<List<T>>() {
        }.getType());
    }

    /**
     * 反序列化 转集合.
     *
     * @param json
     * @param type
     * @param <T>
     * @return 集合实体
     */
    public static final <T> List<T> fromJson2List(String json, Type type) {
        ConvertUtils.checkParam("fromJson2List", json, type, gsonBuild);
        return gsonBuild.fromJson(json, type);
    }

    /**
     * 反序列化 转数组.
     *
     * @param json
     * @param clazz
     * @param <T>
     * @return 集合实体
     */
    public static final <T> List<T> fromJsonList(String json, Class<T[]> clazz) {
        ConvertUtils.checkParam("fromJsonArray", json, gsonBuild);
        return Arrays.asList(gsonBuild.fromJson(json, clazz));
    }

    /**
     * 反序列化 转数组.
     *
     * @param json
     * @param clazz
     * @param <T>
     * @return 数组实体
     */
    public static final <T> T[] fromJsonArray(String json, Class<T[]> clazz) {
        ConvertUtils.checkParam("fromJsonArray", json, gsonBuild);
        return gsonBuild.fromJson(json, clazz);
    }

    /**
     * 对象转换.
     *
     * @param o     数据源
     * @param clazz 目标类型
     * @param <T>
     * @return 目标实体
     */
    public static final <T> T convertToT(Object o, Class<T> clazz) {
        String json = toJson(o);
        return fromJson(json, clazz);
    }

    /**
     * 对象 -> 序列化 -> 反序列化  ->转集合.
     *
     * @param list 必须是集合
     * @param <T>
     * @return 集合实体
     */
    public static final <T> List<T> fromJson2List(List<?> list, Class<T> clazz) {
        ConvertUtils.checkParam("fromJson2List", list, gsonBuild);
        String json = toJson(list);
        return fromJson2List(json, new TypeToken<List<T>>() {
        }.getType());
    }


    /**
     * 对象转换，需要对象中得有相同得变量名才能转换成功.
     * 不支持非静态内部实体类
     *
     * @param objSource  被转换得对象
     * @param objReplica 转换之后得对象
     */
    public static void copyBean(Object objSource, Object objReplica) {
        Field[] fieldsSource = objSource.getClass().getDeclaredFields();
        Field[] fieldsReplica = objReplica.getClass().getDeclaredFields();
        for (Field dataSource : fieldsSource) {
            if (null == dataSource) break;
            dataSource.setAccessible(true);
            String dataSourceName = dataSource.getName();
            if (dataSourceName.isEmpty()) break;
            for (Field dataReplica : fieldsReplica) {
                if (null == dataReplica) break;
                dataReplica.setAccessible(true);
                String dataReplicaName = dataReplica.getName();
                if (dataReplicaName.isEmpty()) break;
                if (dataSourceName.equals(dataReplicaName)) {
                    try {
                        Object obj = dataSource.get(objSource);
                        if (null == obj) break;
                        Object obb = dataReplica.get(objReplica);
                        Type dataReplicaGenericType = dataReplica.getGenericType();
                        String clas = dataReplicaGenericType.toString();
                        if(Logger.openLog) {
                            Logger.d("clas -> ", clas);
                        }
                        if (!isContainsBasicDataType(clas)) {
                            dataReplica.set(objReplica, obj);
                            break;
                        }
                        Object replicaObj = convertDtaReplica(obb, clas, dataReplica, objReplica);
                        convertDataSource(obj, replicaObj);
                    } catch (ClassNotFoundException | InstantiationException |
                             IllegalAccessException var21) {
                        Logger.w(var21.toString());
                    }
                }
            }
        }
    }

    private static void convertDataSource(Object obj, Object replicaObj) throws ClassNotFoundException {
        copyBean(obj, replicaObj);
    }

    private static Object convertDtaReplica(Object obb, String clas, Field dataReplica, Object objReplica) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        if (null == obb) {
            String pathClass = clas.substring(6);
            String className = clas.substring(clas.lastIndexOf(".") + 1);
            if(Logger.openLog) {
                Logger.d("className -> ", pathClass, " className ->", className);
            }
            Class cla = Class.forName(pathClass);
            Object replicaObj = cla.newInstance();
            dataReplica.set(objReplica, replicaObj);
            return replicaObj;
        } else {
            return null;
        }
    }

    private static boolean isContainsBasicDataType(String classType) {
        if (!classType.contains("class")) {
            return false;
        } else {
            String[] var1 = type;
            int var2 = var1.length;

            for (int var3 = 0; var3 < var2; ++var3) {
                String type = var1[var3];
                if (classType.contains(type)) {
                    return false;
                }
            }
            return true;
        }
    }
}
