package blockchain.core

import java.security.interfaces.ECPublicKey
import java.security.spec._
import java.security.{AlgorithmParameters, KeyFactory}

case class CompressedECPublicKey(x: BigInt, y: BigInt, ecPublicKey: ECPublicKey)
    extends ECPublicKey {

  override def getW: ECPoint = ecPublicKey.getW

  override def getAlgorithm: String = ecPublicKey.getAlgorithm

  override def getFormat: String = ecPublicKey.getFormat

  override def getEncoded: Array[Byte] = ecPublicKey.getEncoded

  override def getParams: ECParameterSpec = ecPublicKey.getParams

  def compressed(): Array[Byte] = {
    val xBytes = ecPublicKey.getW.getAffineX.toByteArray
    if ((BigInt(ecPublicKey.getW.getAffineY) mod 2) == 0)
      xBytes.prepended(0x02)
    else
      xBytes.prepended(0x03)
  }

}

object CompressedECPublicKey {

  private val ALGORITHM = "EC"
  private val CURVE_NAME = "secp256k1"

  def fromCompressedPublicKeyBytes(
      compressedPublicKeyBytes: Array[Byte]
  ): CompressedECPublicKey = {
    val algorithmParams = AlgorithmParameters.getInstance(ALGORITHM)
    algorithmParams.init(new ECGenParameterSpec(CURVE_NAME))
    val ecParameterSpec =
      algorithmParams.getParameterSpec(classOf[ECParameterSpec])

    val prefix = compressedPublicKeyBytes(0)
    val x = BigInt(compressedPublicKeyBytes.drop(1))

    val ellipticCurve = ecParameterSpec.getCurve
    val b = ellipticCurve.getB
    val p = BigInt(ellipticCurve.getField.asInstanceOf[ECFieldFp].getP)
    val y = getY(prefix, x, b, p)

    val ecPoint = new ECPoint(x.bigInteger, y.bigInteger)
    val publicKeySpec = new ECPublicKeySpec(ecPoint, ecParameterSpec)

    val kf = KeyFactory.getInstance(ALGORITHM)
    val eCPublicKey = kf.generatePublic(publicKeySpec).asInstanceOf[ECPublicKey]

    CompressedECPublicKey(x, y, eCPublicKey)
  }

  private def getY(prefix: Byte, x: BigInt, b: BigInt, p: BigInt): BigInt = {
    val y = x.pow(3).+(b).bigInteger.modPow((p + 1) / 4, p)
    if ((prefix == 0x02 && y.mod(2) != 0) || (prefix == 0x03 && y.mod(2) == 0))
      p.-(y)
    else y
  }

  def main(args: Array[String]): Unit = {

    val points =
      List(
        (
          BigInt(
            "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798",
            16
          ),
          BigInt(
            "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8",
            16
          )
        ),
        (
          BigInt(
            "C6047F9441ED7D6D3045406E95C07CD85C778E4B8CEF3CA7ABAC09B95C709EE5",
            16
          ),
          BigInt(
            "1AE168FEA63DC339A3C58419466CEAEEF7F632653266D0E1236431A950CFE52A",
            16
          )
        ),
        (
          BigInt(
            "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9",
            16
          ),
          BigInt(
            "388F7B0F632DE8140FE337E62A37F3566500A99934C2231B6CB9FD7584B8E672",
            16
          )
        ),
        (
          BigInt(
            "E493DBF1C10D80F3581E4904930B1404CC6C13900EE0758474FA94ABE8C4CD13",
            16
          ),
          BigInt(
            "51ED993EA0D455B75642E2098EA51448D967AE33BFBDFE40CFE97BDC47739922",
            16
          )
        ),
        (
          BigInt(
            "2F8BDE4D1A07209355B4A7250A5C5128E88B84BDDC619AB7CBA8D569B240EFE4",
            16
          ),
          BigInt(
            "D8AC222636E5E3D6D4DBA9DDA6C9C426F788271BAB0D6840DCA87D3AA6AC62D6",
            16
          )
        ),
        (
          BigInt(
            "FFF97BD5755EEEA420453A14355235D382F6472F8568A18B2F057A1460297556",
            16
          ),
          BigInt(
            "AE12777AACFBB620F3BE96017F45C560DE80F0F6518FE4A03C870C36B075F297",
            16
          )
        ),
        (
          BigInt(
            "5CBDF0646E5DB4EAA398F365F2EA7A0E3D419B7E0330E39CE92BDDEDCAC4F9BC",
            16
          ),
          BigInt(
            "6AEBCA40BA255960A3178D6D861A54DBA813D0B813FDE7B5A5082628087264DA",
            16
          )
        ),
        (
          BigInt(
            "2F01E5E15CCA351DAFF3843FB70F3C2F0A1BDD05E5AF888A67784EF3E10A2A01",
            16
          ),
          BigInt(
            "5C4DA8A741539949293D082A132D13B4C2E213D6BA5B7617B5DA2CB76CBDE904",
            16
          )
        ),
        (
          BigInt(
            "ACD484E2F0C7F65309AD178A9F559ABDE09796974C57E714C35F110DFC27CCBE",
            16
          ),
          BigInt(
            "CC338921B0A7D9FD64380971763B61E9ADD888A4375F8E0F05CC262AC64F9C37",
            16
          )
        ),
        (
          BigInt(
            "A0434D9E47F3C86235477C7B1AE6AE5D3442D49B1943C2B752A68E2A47E247C7",
            16
          ),
          BigInt(
            "893ABA425419BC27A3B6C7E693A24C696F794C2ED877A1593CBEE53B037368D7",
            16
          )
        ),
        (
          BigInt(
            "774AE7F858A9411E5EF4246B70C65AAC5649980BE5C17891BBEC17895DA008CB",
            16
          ),
          BigInt(
            "D984A032EB6B5E190243DD56D7B7B365372DB1E2DFF9D6A8301D74C9C953C61B",
            16
          )
        ),
        (
          BigInt(
            "D01115D548E7561B15C38F004D734633687CF4419620095BC5B0F47070AFE85A",
            16
          ),
          BigInt(
            "A9F34FFDC815E0D7A8B64537E17BD81579238C5DD9A86D526B051B13F4062327",
            16
          )
        ),
        (
          BigInt(
            "F28773C2D975288BC7D1D205C3748651B075FBC6610E58CDDEEDDF8F19405AA8",
            16
          ),
          BigInt(
            "0AB0902E8D880A89758212EB65CDAF473A1A06DA521FA91F29B5CB52DB03ED81",
            16
          )
        ),
        (
          BigInt(
            "499FDF9E895E719CFD64E67F07D38E3226AA7B63678949E6E49B241A60E823E4",
            16
          ),
          BigInt(
            "CAC2F6C4B54E855190F044E4A7B3D464464279C27A3F95BCC65F40D403A13F5B",
            16
          )
        ),
        (
          BigInt(
            "D7924D4F7D43EA965A465AE3095FF41131E5946F3C85F79E44ADBCF8E27E080E",
            16
          ),
          BigInt(
            "581E2872A86C72A683842EC228CC6DEFEA40AF2BD896D3A5C504DC9FF6A26B58",
            16
          )
        ),
        (
          BigInt(
            "E60FCE93B59E9EC53011AABC21C23E97B2A31369B87A5AE9C44EE89E2A6DEC0A",
            16
          ),
          BigInt(
            "F7E3507399E595929DB99F34F57937101296891E44D23F0BE1F32CCE69616821",
            16
          )
        ),
        (
          BigInt(
            "DEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34",
            16
          ),
          BigInt(
            "4211AB0694635168E997B0EAD2A93DAECED1F4A04A95C0F6CFB199F69E56EB77",
            16
          )
        ),
        (
          BigInt(
            "5601570CB47F238D2B0286DB4A990FA0F3BA28D1A319F5E7CF55C2A2444DA7CC",
            16
          ),
          BigInt(
            "C136C1DC0CBEB930E9E298043589351D81D8E0BC736AE2A1F5192E5E8B061D58",
            16
          )
        ),
        (
          BigInt(
            "2B4EA0A797A443D293EF5CFF444F4979F06ACFEBD7E86D277475656138385B6C",
            16
          ),
          BigInt(
            "85E89BC037945D93B343083B5A1C86131A01F60C50269763B570C854E5C09B7A",
            16
          )
        ),
        (
          BigInt(
            "4CE119C96E2FA357200B559B2F7DD5A5F02D5290AFF74B03F3E471B273211C97",
            16
          ),
          BigInt(
            "12BA26DCB10EC1625DA61FA10A844C676162948271D96967450288EE9233DC3A",
            16
          )
        ),
        (
          BigInt(
            "A90CC3D3F3E146DAADFC74CA1372207CB4B725AE708CEF713A98EDD73D99EF29",
            16
          ),
          BigInt(
            "5A79D6B289610C68BC3B47F3D72F9788A26A06868B4D8E433E1E2AD76FB7DC76",
            16
          )
        ),
        (
          BigInt(
            "E5A2636BCFD412EBF36EC45B19BFB68A1BC5F8632E678132B885F7DF99C5E9B3",
            16
          ),
          BigInt(
            "736C1CE161AE27B405CAFD2A7520370153C2C861AC51D6C1D5985D9606B45F39",
            16
          )
        ),
        (
          BigInt(
            "A6B594B38FB3E77C6EDF78161FADE2041F4E09FD8497DB776E546C41567FEB3C",
            16
          ),
          BigInt(
            "71444009192228730CD8237A490FEBA2AFE3D27D7CC1136BC97E439D13330D55",
            16
          )
        ),
        (
          BigInt(
            "00000000000000000000003B78CE563F89A0ED9414F5AA28AD0D96D6795F9C63",
            16
          ),
          BigInt(
            "3F3979BF72AE8202983DC989AEC7F2FF2ED91BDD69CE02FC0700CA100E59DDF3",
            16
          )
        ),
        (
          BigInt(
            "E24CE4BEEE294AA6350FAA67512B99D388693AE4E7F53D19882A6EA169FC1CE1",
            16
          ),
          BigInt(
            "8B71E83545FC2B5872589F99D948C03108D36797C4DE363EBD3FF6A9E1A95B10",
            16
          )
        ),
        (
          BigInt(
            "4CE119C96E2FA357200B559B2F7DD5A5F02D5290AFF74B03F3E471B273211C97",
            16
          ),
          BigInt(
            "ED45D9234EF13E9DA259E05EF57BB3989E9D6B7D8E269698BAFD77106DCC1FF5",
            16
          )
        ),
        (
          BigInt(
            "2B4EA0A797A443D293EF5CFF444F4979F06ACFEBD7E86D277475656138385B6C",
            16
          ),
          BigInt(
            "7A17643FC86BA26C4CBCF7C4A5E379ECE5FE09F3AFD9689C4A8F37AA1A3F60B5",
            16
          )
        ),
        (
          BigInt(
            "5601570CB47F238D2B0286DB4A990FA0F3BA28D1A319F5E7CF55C2A2444DA7CC",
            16
          ),
          BigInt(
            "3EC93E23F34146CF161D67FBCA76CAE27E271F438C951D5E0AE6D1A074F9DED7",
            16
          )
        ),
        (
          BigInt(
            "DEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34",
            16
          ),
          BigInt(
            "BDEE54F96B9CAE9716684F152D56C251312E0B5FB56A3F09304E660861A910B8",
            16
          )
        ),
        (
          BigInt(
            "E60FCE93B59E9EC53011AABC21C23E97B2A31369B87A5AE9C44EE89E2A6DEC0A",
            16
          ),
          BigInt(
            "081CAF8C661A6A6D624660CB0A86C8EFED6976E1BB2DC0F41E0CD330969E940E",
            16
          )
        ),
        (
          BigInt(
            "D7924D4F7D43EA965A465AE3095FF41131E5946F3C85F79E44ADBCF8E27E080E",
            16
          ),
          BigInt(
            "A7E1D78D57938D597C7BD13DD733921015BF50D427692C5A3AFB235F095D90D7",
            16
          )
        ),
        (
          BigInt(
            "499FDF9E895E719CFD64E67F07D38E3226AA7B63678949E6E49B241A60E823E4",
            16
          ),
          BigInt(
            "353D093B4AB17AAE6F0FBB1B584C2B9BB9BD863D85C06A4339A0BF2AFC5EBCD4",
            16
          )
        ),
        (
          BigInt(
            "F28773C2D975288BC7D1D205C3748651B075FBC6610E58CDDEEDDF8F19405AA8",
            16
          ),
          BigInt(
            "F54F6FD17277F5768A7DED149A3250B8C5E5F925ADE056E0D64A34AC24FC0EAE",
            16
          )
        ),
        (
          BigInt(
            "D01115D548E7561B15C38F004D734633687CF4419620095BC5B0F47070AFE85A",
            16
          ),
          BigInt(
            "560CB00237EA1F285749BAC81E8427EA86DC73A2265792AD94FAE4EB0BF9D908",
            16
          )
        ),
        (
          BigInt(
            "774AE7F858A9411E5EF4246B70C65AAC5649980BE5C17891BBEC17895DA008CB",
            16
          ),
          BigInt(
            "267B5FCD1494A1E6FDBC22A928484C9AC8D24E1D20062957CFE28B3536AC3614",
            16
          )
        ),
        (
          BigInt(
            "A0434D9E47F3C86235477C7B1AE6AE5D3442D49B1943C2B752A68E2A47E247C7",
            16
          ),
          BigInt(
            "76C545BDABE643D85C4938196C5DB3969086B3D127885EA6C3411AC3FC8C9358",
            16
          )
        ),
        (
          BigInt(
            "ACD484E2F0C7F65309AD178A9F559ABDE09796974C57E714C35F110DFC27CCBE",
            16
          ),
          BigInt(
            "33CC76DE4F5826029BC7F68E89C49E165227775BC8A071F0FA33D9D439B05FF8",
            16
          )
        ),
        (
          BigInt(
            "2F01E5E15CCA351DAFF3843FB70F3C2F0A1BDD05E5AF888A67784EF3E10A2A01",
            16
          ),
          BigInt(
            "A3B25758BEAC66B6D6C2F7D5ECD2EC4B3D1DEC2945A489E84A25D3479342132B",
            16
          )
        ),
        (
          BigInt(
            "5CBDF0646E5DB4EAA398F365F2EA7A0E3D419B7E0330E39CE92BDDEDCAC4F9BC",
            16
          ),
          BigInt(
            "951435BF45DAA69F5CE8729279E5AB2457EC2F47EC02184A5AF7D9D6F78D9755",
            16
          )
        ),
        (
          BigInt(
            "FFF97BD5755EEEA420453A14355235D382F6472F8568A18B2F057A1460297556",
            16
          ),
          BigInt(
            "51ED8885530449DF0C4169FE80BA3A9F217F0F09AE701B5FC378F3C84F8A0998",
            16
          )
        ),
        (
          BigInt(
            "2F8BDE4D1A07209355B4A7250A5C5128E88B84BDDC619AB7CBA8D569B240EFE4",
            16
          ),
          BigInt(
            "2753DDD9C91A1C292B24562259363BD90877D8E454F297BF235782C459539959",
            16
          )
        ),
        (
          BigInt(
            "E493DBF1C10D80F3581E4904930B1404CC6C13900EE0758474FA94ABE8C4CD13",
            16
          ),
          BigInt(
            "AE1266C15F2BAA48A9BD1DF6715AEBB7269851CC404201BF30168422B88C630D",
            16
          )
        ),
        (
          BigInt(
            "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9",
            16
          ),
          BigInt(
            "C77084F09CD217EBF01CC819D5C80CA99AFF5666CB3DDCE4934602897B4715BD",
            16
          )
        ),
        (
          BigInt(
            "C6047F9441ED7D6D3045406E95C07CD85C778E4B8CEF3CA7ABAC09B95C709EE5",
            16
          ),
          BigInt(
            "E51E970159C23CC65C3A7BE6B99315110809CD9ACD992F1EDC9BCE55AF301705",
            16
          )
        ),
        (
          BigInt(
            "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798",
            16
          ),
          BigInt(
            "B7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777",
            16
          )
        )
      )

    val verify = points.forall { v =>
      val xBytes = v._1.toByteArray

      val compressedECPublicKey1 =
        fromCompressedPublicKeyBytes(xBytes.prepended(0x02))
      val compressedECPublicKey2 =
        fromCompressedPublicKeyBytes(xBytes.prepended(0x03))
      (compressedECPublicKey1.x == v._1 && compressedECPublicKey1.y == v._2) ||
      (compressedECPublicKey2.x == v._1 && compressedECPublicKey2.y == v._2)
    }

    println("is verified")
    println(verify)
  }

}
